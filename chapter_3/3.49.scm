; When the process does not know which locks it needs to acquire,
; before accessing to some shared resource(acquiring another lock).
; So it can be a situation when process #1 acquired a lock A, then
; it finds out all the locks to acquire during operation: B, C, D.
; But there is an another concurrent process #2, which rigth after process #1
; acquired lock A, acquires lock B and finds out all locks to acquire during
; operation: A, E. So when execution of process #1 is resumed, it cannot acquire
; lock B, since if was already acquired by process #2 and process #2 cannot proceed,
; since lock A is acquired by process #1. So the main problem here, that we need to
; guarantee that lock on shared resource is not going to be in results, which this
; resource holds.

