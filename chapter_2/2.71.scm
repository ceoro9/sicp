; Let's take a look an initial set of leafs, specifically on their weights.
; And try to build a huffman tree following by algorightm described in 2.68.
;
; 1. { 1 2 4 8 ... 2^n-1 }
;
; 2. { {1, 2}, 4, 8 ... 2^n-1}
; Current step Haffman tree:
;    {1, 2}
;   /     \
;  {1}    {2}
;
; 3. { {1, 2, 4}, 8 ... 2^n-1}
; Current step Haffman tree:
;
;    {1, 2, 4}
;    /       \
;   {4}     {1, 2}
;           /    \
;          {1}   {2}
;
; and etc. ...
;
; So the reduction is absolutely consistient
; (first and second elements of set is combined into one 
; and the combined element takes the first position in set),
; because inequality below is always true with any N > 1.
;
;  n - 1
;  SUM   2^i < 2^n
;  i = 0
;
; So sum of weights of n-2 and n-1 elements 
; never exceeds the weight of n element. 
; Prove of this is equality is really basic:
;
; This ineqality is true:
;  
;  n - 1
;  SUM   2^i < 2^n
;  i = 0
;
; when this one is true
;
;  n - 2
;  SUM   2^i < 2^(n-1)
;  i = 0
;
; Becasue if we re-write our inequality in other format:
;
; (2^0 + 2^1 + ... + 2^(n-2)) + 2^(n-1) < 2^n
;
; We can see that to break the given inequality, (2^0 + 2^1 ... + 2^(n-1))
; should be greater than 2^(n-1) and as well greater than half of 2^n. So
; there is a recursive dependency. So to prove the inequality we need to consider
; the basic case of recursion, if it's true, it means that all of others 
; are true as well, as it was described above. The basic case is:
;
; 2^0 + 2^1 < 2^2
; 1 + 2 < 4
;
; So obviously it's true, it means that
;
;  n - 1
;  SUM   2^i < 2^n
;  i = 0
;
; it true as well.
;
; The result tree will look like something like this:
;
;    {1, 2, 4}
;    /       \
;   {4}     {1, 2}
;           /    \
;          {1}   {2}
;
; And answers on posed questions are pretty straigh-foward:
;
; How many bits are required to encode the most frequent symbol?
; answer: 1
;
; How many bits are required to encode least frequent symbol?
; answer: n - 1
; ------
; (because number of edges to pass between root and leaf
;  in such tree is N - 1, but depth of tree is N, 2 vertexes are
;  connected with 1 edge, so number of edges are less on 1 than number of vertexes)
; ------

