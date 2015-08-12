(ns play.core
  (:require [clojure.core.async :as async
             :refer [<! >! <!! >!! alt! alts! alts!! chan close! go go-loop onto-chan pipe put! take! timeout]]))

;===============================================================================
; This is an exploration of core.async for learning purposes.

(defn delayed-hello-world []
  (println "Starting")
  (let [my-q (chan 5)
        my-seq (seq "Hello World")]
    (go
      (doseq [item my-seq]
        (>! my-q item)
        (<! (timeout 1000))))
    #_(go
        (while true
          (let [c (<! my-q)]
            (println c))))
    (go-loop []
      (when-let [c (<! my-q)]
        (println c)
        (recur)))
    (future (Thread/sleep 12000)
            (println "-"))
    (shutdown-agents)
    (println "Finished")))

(defn interleaved-hello-world []
  (let [q1 (chan 5)
        q2 (chan 5)
        seq1 (seq "Hello World")
        seq2 (range 10)]
    (go
      (doseq [item seq1]
        (>! q1 item)
        (<! (timeout 1000))))
    (go
      (doseq [item seq2]
        (>! q2 item)
        (<! (timeout (rand-int 1500)))))
    (go
      (while true
        (let [[item queue] (alts! [q1 q2])]
          (condp = queue
            q1 (println item)
            q2 (println " " item)))))))

(defn lots-of-channels []
  (let [begin (System/currentTimeMillis)
        n 1000
        cs (repeatedly n chan)]
    (doseq [c cs] (go (>! c "hi")))
    (dotimes [i n]
      (let [[v c] (alts!! cs)]
        (assert (= "hi" v))))
    (println "Read" n "msgs in" (- (System/currentTimeMillis) begin) "ms")))

(comment
  (delayed-hello-world)
  (interleaved-hello-world)
  (lots-of-channels))

;===============================================================================

#_(defn foo [in]
  (go-loop [v (<! in)]
    (when v
      (println v)
      (recur (<! in)))))

;===============================================================================

(defn chan-of-primes-naive []
  (let [ints (chan)
        primes (chan)]
    (go-loop [n 2]
      (>! ints n)
      (recur (inc n)))
    (go-loop [cur-ch ints]
      (let [prime (<! cur-ch)
            new-ch (chan 1 (filter #(not= 0 (mod % prime))))]
        (>! primes prime)
        (pipe cur-ch new-ch)
        (recur new-ch)))
    primes))

(defn chan-of-ints-old [xform start-n]
  (let [ints (chan 1 xform)]
    (go-loop [n start-n]
      (>! ints n)
      (recur (inc n)))
    ints))

(defn chan-of-ints [start-n]
  (let [ints (chan)]
    (go-loop [n start-n]
      (>! ints n)
      (recur (inc n)))
    ints))

(defn posmod-sift []
  (fn [rf]
    (let [seen (volatile! [])]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (if (every? #(pos? (mod input %)) @seen)
           (do (vswap! seen conj input)
               (rf result input))
           result))))))

(defn chan-of-primes-pipe []
  (let [inputs (chan-of-ints 2)
        primes (chan 1 (posmod-sift))]
    (pipe inputs primes)
    primes))

(defn chan-of-primes-onto []
  (let [inputs (drop 2 (range))
        primes (chan 1 (posmod-sift))]
    (onto-chan primes inputs)
    primes))

(defn chan-of-primes-loop []
  (let [inputs (drop 2 (range))
        primes (chan 1 (posmod-sift))]
    (go-loop [vs inputs]
      (when (>! primes (first vs))
        (recur (rest vs))))
    primes))

(def primes (chan-of-primes-loop))

(defn consume [n ch]
  (dorun (repeatedly n #(<!! ch))))

(defn pipe-xf [ch xf]
  (let [to-ch (chan 1 xf)]
    (pipe ch to-ch)
    to-ch))

(comment
  (<!! primes)
  (<!! primes-x)
  (time (consume 8000 (chan-of-primes-naive)))
  (time (consume 11000 (chan-of-primes-pipe)))
  (time (consume 11000 (chan-of-primes-onto)))
  (time (consume 11000 (chan-of-primes-loop)))
  (time (consume 4000 (chan-of-primes-old)))
  (time (count (transduce (posmod-sift) conj (cons 2 (range 3 10000 2)))))
  (time (count (transduce (posmod-sift) conj (cons 2 (range 3 10000)))))
  (time (count (transduce (posmod-sift) conj (range 3 10000))))
  (time (count (transduce (posmod-sift) conj (range 3 10000 2))))
  )

