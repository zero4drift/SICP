(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))				;under applicative order this leads to dead loop when eval (p)
					;under normal order this return 0 as result

;; (define (p) (p)) 定义了一个无参数过程 p，过程体就是调用它自己，对 (p) 进行求
;; 值会进入无限递归进而报错；

;; 应用序：报错，因为在应用序下要对组合式的各个子表达
;; 式进行求值，对第二个参数表达式求值将会触发上面第一条所讲的情况；

;; 正则序：通过，因为正则序求值是直到实际需要运算对象的值时再去做，而定义的 test
;; 过程在传入的第一个参数为0的情况下过程体谓词部分的值为 true, 根据 if 条件表达
;; 式的特殊求值规则，它包含 (p) 的 <alternative> 部分并不求值，所以不会触发第一
;; 条所讲的情况。
