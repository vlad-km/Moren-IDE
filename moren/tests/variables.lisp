(batch-begin "JSCL Variables in an browser asynchronous environment")

(defvar *x*)
(setq *x* 0)

(let* ((*x* (progn
              (test (= *x* 0))
              (setq *x* 1)
              (test (= *x* 1))
              (test (= *x* 0))
              2)))
  (expected-failure (= *x* 2)))

(expected-failure (= *x* 1))

(test (= *x* 0))

(batch-end)