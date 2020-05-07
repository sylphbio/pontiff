(module test.integration ()

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)

(import test)

(test-group "basic tests"
  (test "lgtm!" 4 (+ 2 2)))

(test-exit)

)
