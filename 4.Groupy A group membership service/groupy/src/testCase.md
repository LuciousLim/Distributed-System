W1 = test:first(1, gms3, 1000).
W2 = test:add(2, gms3, W1, 1000).
W3 = test:add(3, gms3, W1, 1000).
W4 = test:add(4, gms3, W, 1000).
W5 = test:add(5, gms3, W, 1000).
W6 = test:add(6, gms3, W, 1000).


W1 = test:first(1, gms4, 1000).
W2 = test:add(2, gms4, W1, 1000).
W3 = test:add(3, gms4, W1, 1000).
W4 = test:add(4, gms4, W, 1000).
W5 = test:add(5, gms4, W, 1000).
W6 = test:add(6, gms4, W, 1000).

test:more(6,gms4,1000).
