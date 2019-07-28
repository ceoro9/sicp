; > Is this a safe change to make?
; Absolutely yes. It does not really matter whether 2 concurrenly serilized functions
; are the same or not, since both of them are serialized under the same serializer,
; one cannot be interleaved with another one. 
