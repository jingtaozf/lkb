#+:allegro
(progn (format t "~A" "Hello") nil)

#-:allegro
(progn (format t "~A" "Goodbye") nil)

#+:allegro
(progn (format t "~A" "Hello again") nil)
