(let ((x 1))
   (letrec ((x 2))
     x)

   (let* ((x 3))
     x)

   (letrec ((x 4))
     x)
   x)
