(ns play.sieve
  (:require [clojure.core.async :as async
             :refer [<! >! <!! >!! alt! alts! alts!! chan close! go go-loop pipe put! timeout]]))

;===============================================================================

;;; concurrent prime sieve in Clojure using core.async
;; inspired by a similar implementation in Go
;; http://golang.org/doc/play/sieve.go

(defmacro go-forever
  "An infinite loop that runs in a go block."
  [& body]
  `(go (loop []
         ~@body
         (recur))))

(defn filter-chan
  "Somewhat like clojure.core/filter, but operates on a
   channel and returns a new channel. "
  [pred ch]
  (let [out (chan)]
    (go-forever
      (let [val (<! ch)]
        (when (pred val)
          (>! out val))))
    out))

(defn generate
  "Generate all numbers into channel."
  [ch]
  (go-loop [n 2]
    (>! ch n)
    (recur (inc n))))

(defn sieve-beta
  "Generate n primes via a concurrent prime sieve.
   Successive prime filtering is done by daisy-chaining channels."
  [n]
  (let [in (chan)]
    (generate in)
    (go-loop [i 0
              ch in]
      (when (< i n)
        (let [prime (<! ch)
              nxt (filter-chan #(pos? (rem % prime)) ch)]
          (println prime)
          (recur (inc i) nxt))))
    :ok))

(comment
  (time (sieve-beta 1000)))

;===============================================================================

;(defn prime-filter [in primes]
;  (go
;    (let [prime (<! in)
;          out (filter< #(not= (mod % prime) 0) in)]
;      (>! primes prime)
;      (prime-filter out primes))))
;
;(def sieve-gamma
;  (let [in (chan)
;        out (chan)]
;    (prime-filter in out)
;    (go-loop [n 2]
;      (>! in n)
;      (recur (inc n)))
;    out))
;
;(def gamma-primes (repeatedly #(<!! sieve-gamma)))
;
;(comment
;  (time (take 1000 gamma-primes)))

;===============================================================================

(defn filter-divisible [div] (filter (fn [x] (not= 0 (mod x div)))))

(defn sieve-alpha []
  (let [c (chan)]
    (go (doseq [x (take 1000 (iterate inc 2))]
          (>! c x)))
    (go-loop [cur-chan c]
      (let [prime (<! cur-chan)
            new-chan (chan 1 (filter-divisible prime))]
        (println prime)
        (pipe cur-chan new-chan)
        (recur new-chan)))))

(comment
  (time (sieve-alpha)))

;===============================================================================

;(defn factor? [x y]
;  (zero? (mod y x)))
;
;(defn get-primes [limit]
;  (let [primes (chan)
;        numbers (to-chan (range 2 limit))]
;    (go-loop [ch numbers]
;      (when-let [prime (<! ch)]
;        (>! primes prime)
;        (recur (remove< (partial factor? prime) ch)))
;      (close! primes))
;    primes))
;
;(defn -main [limit]
;  (let [primes (get-primes (edn/read-string limit))]
;    (loop []
;      (when-let [prime (<!! primes)]
;        (println prime)
;        (recur)))))

;===============================================================================

