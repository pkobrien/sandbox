# -*- coding: utf-8 -*-
"""
@author: Patrick K. O'Brien

A simple Signal class that is similar to PyQt signals and slots.
"""

class Signal(object):
    """Notifies connected slots when emit() is called."""

    def __init__(self):
        self._slots = set()

    def emit(self, *args, **kwargs):
        """Call all connected slots."""
        for slot in self._slots:
            slot(*args, **kwargs)

    def connect(self, slot):
        """Add slot to set of connected slots."""
        self._slots.add(slot)

    def disconnect(self, slot):
        """Remove slot from set of connected slots, if it is a member."""
        self._slots.discard(slot)
