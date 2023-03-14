; Changes:
; * removed: 4
; * added: 4
; * swaps: 4
; * negated predicates: 1
; * swapped branches: 1
; * calls to id fun: 8
(letrec ((false #f)
         (true #t)
         (make-machine (lambda (register-names ops controller-text)
                         (let ((machine (make-new-machine)))
                            (<change>
                               (for-each (lambda (register-name) ((machine 'allocate-register) register-name)) register-names)
                               ((machine 'install-operations) ops))
                            (<change>
                               ((machine 'install-operations) ops)
                               (for-each
                                  (lambda (register-name)
                                     'allocate-register
                                     ((machine 'allocate-register) register-name))
                                  register-names))
                            (<change>
                               ((machine 'install-instruction-sequence) (assemble controller-text machine))
                               ())
                            machine)))
         (make-register (lambda (name)
                          (let ((contents '*unassigned*))
                             (letrec ((dispatch (lambda (message)
                                                  (if (eq? message 'get)
                                                     contents
                                                     (if (eq? message 'set)
                                                        (lambda (value)
                                                           (set! contents value))
                                                        (error "Unknown request -- REGISTER" message))))))
                                dispatch))))
         (get-contents (lambda (register)
                         (register 'get)))
         (set-contents! (lambda (register value)
                          ((register 'set) value)))
         (make-stack (lambda ()
                       (let ((s ()))
                          (letrec ((push (lambda (x)
                                           (set! s (cons x s))))
                                   (pop (lambda ()
                                          (if (null? s)
                                             (error "Empty stack -- POP")
                                             (let ((top (car s)))
                                                (set! s (cdr s))
                                                top))))
                                   (initialize (lambda ()
                                                 (set! s ())
                                                 'done))
                                   (dispatch (lambda (message)
                                               (if (eq? message 'push)
                                                  push
                                                  (if (eq? message 'pop)
                                                     (pop)
                                                     (if (eq? message 'initialize)
                                                        (initialize)
                                                        (error "Unknown request -- STACK" message)))))))
                             dispatch))))
         (pop (lambda (stack)
                (stack 'pop)))
         (push (lambda (stack value)
                 ((stack 'push) value)))
         (make-new-machine (lambda ()
                             (let ((pc (make-register 'pc))
                                   (flag (make-register 'flag))
                                   (stack (make-stack))
                                   (the-instruction-sequence ()))
                                (let ((the-ops (list
                                                 (list 'initialize-stack (lambda () (stack 'initialize)))
                                                 (list 'print-stack-statistics (lambda () (stack 'print-statistics)))))
                                      (register-table (list (list 'pc pc) (list 'flag flag))))
                                   (letrec ((allocate-register (lambda (name)
                                                                 (if (assoc name register-table)
                                                                    (error "Multiply defined register: " name)
                                                                    (set! register-table (cons (list name (make-register name)) register-table)))
                                                                 'register-allocated))
                                            (lookup-register (lambda (name)
                                                               (let ((val (assoc name register-table)))
                                                                  (if val
                                                                     (cadr val)
                                                                     (error "Unknown register:" name)))))
                                            (execute (lambda ()
                                                       (let ((insts (get-contents pc)))
                                                          (if (null? insts)
                                                             'done
                                                             (begin
                                                                ((instruction-execution-proc (car insts)))
                                                                (execute))))))
                                            (dispatch (lambda (message)
                                                        (if (eq? message 'start)
                                                           (begin
                                                              (set-contents! pc the-instruction-sequence)
                                                              (execute))
                                                           (if (eq? message 'install-instruction-sequence)
                                                              (lambda (seq)
                                                                 (set! the-instruction-sequence seq))
                                                              (if (eq? message 'allocate-register)
                                                                 allocate-register
                                                                 (if (eq? message 'get-register)
                                                                    lookup-register
                                                                    (if (eq? message 'install-operations)
                                                                       (lambda (ops)
                                                                          (set! the-ops (append the-ops ops)))
                                                                       (if (eq? message 'stack)
                                                                          stack
                                                                          (if (eq? message 'operations)
                                                                             the-ops
                                                                             (error "Unknown request -- MACHINE" message)))))))))))
                                      dispatch)))))
         (start (lambda (machine)
                  (<change>
                     (machine 'start)
                     ((lambda (x) x) (machine 'start)))))
         (get-register-contents (lambda (machine register-name)
                                  (get-contents (get-register machine register-name))))
         (set-register-contents! (lambda (machine register-name value)
                                   (<change>
                                      ()
                                      (display register-name))
                                   (set-contents! (get-register machine register-name) value)
                                   'done))
         (get-register (lambda (machine reg-name)
                         ((machine 'get-register) reg-name)))
         (assemble (lambda (controller-text machine)
                     (<change>
                        (let ((result (extract-labels controller-text)))
                           (let ((insts (car result))
                                 (labels (cdr result)))
                              (update-insts! insts labels machine)
                              insts))
                        ((lambda (x) x)
                           (let ((result (extract-labels controller-text)))
                              (<change>
                                 ()
                                 result)
                              (let ((insts (car result))
                                    (labels (cdr result)))
                                 (update-insts! insts labels machine)
                                 insts))))))
         (extract-labels (lambda (text)
                           (if (null? text)
                              (cons () ())
                              (let ((result (extract-labels (cdr text))))
                                 (let ((insts (car result))
                                       (labels (cdr result)))
                                    (let ((next-inst (car text)))
                                       (if (symbol? next-inst)
                                          (cons insts (cons (make-label-entry next-inst insts) labels))
                                          (cons (cons (make-instruction next-inst) insts) labels))))))))
         (update-insts! (lambda (insts labels machine)
                          (let ((pc (get-register machine 'pc))
                                (flag (get-register machine 'flag))
                                (stack (machine 'stack))
                                (ops (machine 'operations)))
                             (for-each
                                (lambda (inst)
                                   (set-instruction-execution-proc!
                                      inst
                                      (make-execution-procedure (instruction-text inst) labels machine pc flag stack ops)))
                                insts))))
         (make-instruction (lambda (text)
                             (cons text ())))
         (instruction-text (lambda (inst)
                             (<change>
                                (car inst)
                                ((lambda (x) x) (car inst)))))
         (instruction-execution-proc (lambda (inst)
                                       (cdr inst)))
         (set-instruction-execution-proc! (lambda (inst proc)
                                            (set-cdr! inst proc)))
         (make-label-entry (lambda (label-name insts)
                             (<change>
                                (cons label-name insts)
                                ((lambda (x) x) (cons label-name insts)))))
         (lookup-label (lambda (labels label-name)
                         (let ((val (assoc label-name labels)))
                            (if val
                               (cdr val)
                               (error "Undefined label -- ASSEMBLE" label-name)))))
         (make-execution-procedure (lambda (inst labels machine pc flag stack ops)
                                     (if (eq? (car inst) 'assign)
                                        (make-assign inst machine labels ops pc)
                                        (if (eq? (car inst) 'test)
                                           (make-test inst machine labels ops flag pc)
                                           (if (eq? (car inst) 'branch)
                                              (make-branch inst machine labels flag pc)
                                              (if (eq? (car inst) 'goto)
                                                 (make-goto inst machine labels pc)
                                                 (if (eq? (car inst) 'save)
                                                    (make-save inst machine stack pc)
                                                    (if (eq? (car inst) 'restore)
                                                       (make-restore inst machine stack pc)
                                                       (if (eq? (car inst) 'perform)
                                                          (make-perform inst machine labels ops pc)
                                                          (error "Unknown instruction type -- ASSEMBLE" inst))))))))))
         (make-assign (lambda (inst machine labels operations pc)
                        (let ((target (get-register machine (assign-reg-name inst)))
                              (value-exp (assign-value-exp inst)))
                           (let ((value-proc (if (<change> (operation-exp? value-exp) (not (operation-exp? value-exp)))
                                               (make-operation-exp value-exp machine labels operations)
                                               (make-primitive-exp (car value-exp) machine labels))))
                              (lambda ()
                                 (set-contents! target (value-proc))
                                 (advance-pc pc))))))
         (assign-reg-name (lambda (assign-instruction)
                            (cadr assign-instruction)))
         (assign-value-exp (lambda (assign-instruction)
                             (cddr assign-instruction)))
         (advance-pc (lambda (pc)
                       (set-contents! pc (cdr (get-contents pc)))))
         (make-test (lambda (inst machine labels operations flag pc)
                      (let ((condition (test-condition inst)))
                         (if (operation-exp? condition)
                            (let ((condition-proc (make-operation-exp condition machine labels operations)))
                               (lambda ()
                                  (set-contents! flag (condition-proc))
                                  (advance-pc pc)))
                            (error "Bad TEST instruction -- ASSEMBLE" inst)))))
         (test-condition (lambda (test-instruction)
                           (cdr test-instruction)))
         (make-branch (lambda (inst machine labels flag pc)
                        (let ((dest (branch-dest inst)))
                           (if (label-exp? dest)
                              (let ((insts (lookup-label labels (label-exp-label dest))))
                                 (lambda ()
                                    (if (get-contents flag)
                                       (set-contents! pc insts)
                                       (advance-pc pc))))
                              (error "Bad BRANCH instruction -- ASSEMBLE" inst)))))
         (branch-dest (lambda (branch-instruction)
                        (cadr branch-instruction)))
         (make-goto (lambda (inst machine labels pc)
                      (let ((dest (goto-dest inst)))
                         (if (label-exp? dest)
                            (let ((insts (lookup-label labels (label-exp-label dest))))
                               (lambda ()
                                  (set-contents! pc insts)))
                            (if (register-exp? dest)
                               (let ((reg (get-register machine (register-exp-reg dest))))
                                  (lambda ()
                                     (set-contents! pc (get-contents reg))))
                               (error "Bad GOTO instruction -- ASSEMBLE" inst))))))
         (goto-dest (lambda (goto-instruction)
                      (cadr goto-instruction)))
         (make-save (lambda (inst machine stack pc)
                      (let ((reg (get-register machine (stack-inst-reg-name inst))))
                         (lambda ()
                            (push stack (get-contents reg))
                            (advance-pc pc)))))
         (make-restore (lambda (inst machine stack pc)
                         (let ((reg (get-register machine (stack-inst-reg-name inst))))
                            (<change>
                               ()
                               pc)
                            (lambda ()
                               (<change>
                                  (set-contents! reg (pop stack))
                                  ((lambda (x) x) (set-contents! reg (pop stack))))
                               (advance-pc pc)))))
         (stack-inst-reg-name (lambda (stack-instruction)
                                (cadr stack-instruction)))
         (make-perform (lambda (inst machine labels operations pc)
                         (let ((action (perform-action inst)))
                            (if (operation-exp? action)
                               (let ((action-proc (make-operation-exp action machine labels operations)))
                                  (lambda ()
                                     (action-proc)
                                     (advance-pc pc)))
                               (error "Bad PERFORM instruction -- ASSEMBLE" inst)))))
         (perform-action (lambda (inst)
                           (cdr inst)))
         (make-primitive-exp (lambda (exp machine labels)
                               (if (constant-exp? exp)
                                  (let ((c (constant-exp-value exp)))
                                     (lambda ()
                                        c))
                                  (if (label-exp? exp)
                                     (let ((insts (lookup-label labels (label-exp-label exp))))
                                        (lambda ()
                                           insts))
                                     (if (register-exp? exp)
                                        (let ((r (get-register machine (register-exp-reg exp))))
                                           (lambda ()
                                              (get-contents r)))
                                        (error "Unknown expression type -- ASSEMBLE" exp))))))
         (register-exp? (lambda (exp)
                          (tagged-list? exp 'reg)))
         (register-exp-reg (lambda (exp)
                             (cadr exp)))
         (constant-exp? (lambda (exp)
                          (tagged-list? exp 'const)))
         (constant-exp-value (lambda (exp)
                               (cadr exp)))
         (label-exp? (lambda (exp)
                       (tagged-list? exp 'label)))
         (label-exp-label (lambda (exp)
                            (cadr exp)))
         (make-operation-exp (lambda (exp machine labels operations)
                               (let ((op (lookup-prim (operation-exp-op exp) operations))
                                     (aprocs (map (lambda (e) (make-primitive-exp e machine labels)) (operation-exp-operands exp))))
                                  (lambda ()
                                     (if (null? aprocs)
                                        (op)
                                        (if (null? (cddr aprocs))
                                           (<change>
                                              (op ((car aprocs)) ((cadr aprocs)))
                                              (if (null? (cdr aprocs))
                                                 (op ((car aprocs)))
                                                 (error "apply")))
                                           (<change>
                                              (if (null? (cdr aprocs))
                                                 (op ((car aprocs)))
                                                 (error "apply"))
                                              (op ((car aprocs)) ((cadr aprocs))))))))))
         (operation-exp? (lambda (exp)
                           (if (pair? exp) (tagged-list? (car exp) 'op) #f)))
         (operation-exp-op (lambda (operation-exp)
                             (<change>
                                (cadr (car operation-exp))
                                ((lambda (x) x) (cadr (car operation-exp))))))
         (operation-exp-operands (lambda (operation-exp)
                                   (cdr operation-exp)))
         (lookup-prim (lambda (symbol operations)
                        (let ((val (assoc symbol operations)))
                           (if val
                              (cadr val)
                              (error "Unknown operation -- ASSEMBLE" symbol)))))
         (tagged-list? (lambda (exp tag)
                         (if (pair? exp) (eq? (car exp) tag) false)))
         (ops (__toplevel_cons
                (__toplevel_cons '+ (__toplevel_cons + ()))
                (__toplevel_cons
                   (__toplevel_cons '* (__toplevel_cons * ()))
                   (__toplevel_cons
                      (__toplevel_cons '- (__toplevel_cons - ()))
                      (__toplevel_cons
                         (__toplevel_cons '= (__toplevel_cons = ()))
                         (__toplevel_cons
                            (__toplevel_cons '< (__toplevel_cons < ()))
                            (__toplevel_cons
                               (__toplevel_cons '> (__toplevel_cons > ()))
                               (__toplevel_cons (__toplevel_cons 'display (__toplevel_cons (lambda (x) (display x)) ())) ())))))))))
   (let ((gcd-machine (make-machine
                        (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 't ())))
                        ops
                        (__toplevel_cons
                           'test-b
                           (__toplevel_cons
                              (__toplevel_cons
                                 'test
                                 (__toplevel_cons
                                    (__toplevel_cons 'op (__toplevel_cons '= ()))
                                    (__toplevel_cons
                                       (__toplevel_cons 'reg (__toplevel_cons 'b ()))
                                       (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 0 ())) ()))))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'branch
                                    (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'gcd-done ())) ()))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'assign
                                       (__toplevel_cons 't (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'a ())) ())))
                                    (__toplevel_cons
                                       'rem-loop
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'test
                                             (__toplevel_cons
                                                (__toplevel_cons 'op (__toplevel_cons '< ()))
                                                (__toplevel_cons
                                                   (__toplevel_cons 'reg (__toplevel_cons 't ()))
                                                   (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'b ())) ()))))
                                          (__toplevel_cons
                                             (__toplevel_cons
                                                'branch
                                                (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'rem-done ())) ()))
                                             (__toplevel_cons
                                                (__toplevel_cons
                                                   'assign
                                                   (__toplevel_cons
                                                      't
                                                      (__toplevel_cons
                                                         (__toplevel_cons 'op (__toplevel_cons '- ()))
                                                         (__toplevel_cons
                                                            (__toplevel_cons 'reg (__toplevel_cons 't ()))
                                                            (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'b ())) ())))))
                                                (__toplevel_cons
                                                   (__toplevel_cons
                                                      'goto
                                                      (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'rem-loop ())) ()))
                                                   (__toplevel_cons
                                                      'rem-done
                                                      (__toplevel_cons
                                                         (__toplevel_cons
                                                            'assign
                                                            (__toplevel_cons 'a (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'b ())) ())))
                                                         (__toplevel_cons
                                                            (__toplevel_cons
                                                               'assign
                                                               (__toplevel_cons 'b (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 't ())) ())))
                                                            (__toplevel_cons
                                                               (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'test-b ())) ()))
                                                               (__toplevel_cons 'gcd-done ())))))))))))))))))
      (display "(gcd 10 15): ")
      (set-register-contents! gcd-machine 'a 10)
      (<change>
         (set-register-contents! gcd-machine 'b 15)
         ())
      (<change>
         (start gcd-machine)
         (display (get-register-contents gcd-machine 'a)))
      (<change>
         (display (get-register-contents gcd-machine 'a))
         (start gcd-machine))
      (newline))
   (<change>
      (let ((fac-machine (make-machine
                           (__toplevel_cons 'continue (__toplevel_cons 'n (__toplevel_cons 'val ())))
                           ops
                           (__toplevel_cons
                              'start
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'assign
                                    (__toplevel_cons
                                       'continue
                                       (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'fact-done ())) ())))
                                 (__toplevel_cons
                                    'fact-loop
                                    (__toplevel_cons
                                       (__toplevel_cons
                                          'test
                                          (__toplevel_cons
                                             (__toplevel_cons 'op (__toplevel_cons '= ()))
                                             (__toplevel_cons
                                                (__toplevel_cons 'reg (__toplevel_cons 'n ()))
                                                (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 1 ())) ()))))
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'branch
                                             (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'base-case ())) ()))
                                          (__toplevel_cons
                                             (__toplevel_cons 'save (__toplevel_cons 'continue ()))
                                             (__toplevel_cons
                                                (__toplevel_cons 'save (__toplevel_cons 'n ()))
                                                (__toplevel_cons
                                                   (__toplevel_cons
                                                      'assign
                                                      (__toplevel_cons
                                                         'n
                                                         (__toplevel_cons
                                                            (__toplevel_cons 'op (__toplevel_cons '- ()))
                                                            (__toplevel_cons
                                                               (__toplevel_cons 'reg (__toplevel_cons 'n ()))
                                                               (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 1 ())) ())))))
                                                   (__toplevel_cons
                                                      (__toplevel_cons
                                                         'assign
                                                         (__toplevel_cons
                                                            'continue
                                                            (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'after-fact ())) ())))
                                                      (__toplevel_cons
                                                         (__toplevel_cons
                                                            'goto
                                                            (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'fact-loop ())) ()))
                                                         (__toplevel_cons
                                                            'after-fact
                                                            (__toplevel_cons
                                                               (__toplevel_cons 'restore (__toplevel_cons 'n ()))
                                                               (__toplevel_cons
                                                                  (__toplevel_cons 'restore (__toplevel_cons 'continue ()))
                                                                  (__toplevel_cons
                                                                     (__toplevel_cons
                                                                        'assign
                                                                        (__toplevel_cons
                                                                           'val
                                                                           (__toplevel_cons
                                                                              (__toplevel_cons 'op (__toplevel_cons '* ()))
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons 'reg (__toplevel_cons 'n ()))
                                                                                 (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ())))))
                                                                     (__toplevel_cons
                                                                        (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'continue ())) ()))
                                                                        (__toplevel_cons
                                                                           'base-case
                                                                           (__toplevel_cons
                                                                              (__toplevel_cons
                                                                                 'assign
                                                                                 (__toplevel_cons 'val (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 1 ())) ())))
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'continue ())) ()))
                                                                                 (__toplevel_cons 'fact-done ()))))))))))))))))))))))
         (display "(fac 5): ")
         (set-register-contents! fac-machine 'n 5)
         (start fac-machine)
         (display (get-register-contents fac-machine 'val))
         (newline))
      (let ((fib-machine (make-machine
                           (__toplevel_cons 'continue (__toplevel_cons 'n (__toplevel_cons 'val ())))
                           ops
                           (__toplevel_cons
                              'start
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'assign
                                    (__toplevel_cons
                                       'continue
                                       (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'fib-done ())) ())))
                                 (__toplevel_cons
                                    'fib-loop
                                    (__toplevel_cons
                                       (__toplevel_cons
                                          'test
                                          (__toplevel_cons
                                             (__toplevel_cons 'op (__toplevel_cons '< ()))
                                             (__toplevel_cons
                                                (__toplevel_cons 'reg (__toplevel_cons 'n ()))
                                                (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 2 ())) ()))))
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'branch
                                             (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'immediate-answer ())) ()))
                                          (__toplevel_cons
                                             (__toplevel_cons 'save (__toplevel_cons 'continue ()))
                                             (__toplevel_cons
                                                (__toplevel_cons 'save (__toplevel_cons 'n ()))
                                                (__toplevel_cons
                                                   (__toplevel_cons
                                                      'assign
                                                      (__toplevel_cons
                                                         'continue
                                                         (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'afterfib-n-1 ())) ())))
                                                   (__toplevel_cons
                                                      (__toplevel_cons
                                                         'assign
                                                         (__toplevel_cons
                                                            'n
                                                            (__toplevel_cons
                                                               (__toplevel_cons 'op (__toplevel_cons '- ()))
                                                               (__toplevel_cons
                                                                  (__toplevel_cons 'reg (__toplevel_cons 'n ()))
                                                                  (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 1 ())) ())))))
                                                      (__toplevel_cons
                                                         (__toplevel_cons
                                                            'goto
                                                            (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'fib-loop ())) ()))
                                                         (__toplevel_cons
                                                            'afterfib-n-1
                                                            (__toplevel_cons
                                                               (__toplevel_cons 'restore (__toplevel_cons 'n ()))
                                                               (__toplevel_cons
                                                                  (__toplevel_cons 'restore (__toplevel_cons 'continue ()))
                                                                  (__toplevel_cons
                                                                     (__toplevel_cons
                                                                        'assign
                                                                        (__toplevel_cons
                                                                           'n
                                                                           (__toplevel_cons
                                                                              (__toplevel_cons 'op (__toplevel_cons '- ()))
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons 'reg (__toplevel_cons 'n ()))
                                                                                 (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 2 ())) ())))))
                                                                     (__toplevel_cons
                                                                        (__toplevel_cons 'save (__toplevel_cons 'continue ()))
                                                                        (__toplevel_cons
                                                                           (__toplevel_cons
                                                                              'assign
                                                                              (__toplevel_cons
                                                                                 'continue
                                                                                 (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'afterfib-n-2 ())) ())))
                                                                           (__toplevel_cons
                                                                              (__toplevel_cons 'save (__toplevel_cons 'val ()))
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons
                                                                                    'goto
                                                                                    (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'fib-loop ())) ()))
                                                                                 (__toplevel_cons
                                                                                    'afterfib-n-2
                                                                                    (__toplevel_cons
                                                                                       (__toplevel_cons
                                                                                          'assign
                                                                                          (__toplevel_cons 'n (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ())))
                                                                                       (__toplevel_cons
                                                                                          (__toplevel_cons 'restore (__toplevel_cons 'val ()))
                                                                                          (__toplevel_cons
                                                                                             (__toplevel_cons 'restore (__toplevel_cons 'continue ()))
                                                                                             (__toplevel_cons
                                                                                                (__toplevel_cons
                                                                                                   'assign
                                                                                                   (__toplevel_cons
                                                                                                      'val
                                                                                                      (__toplevel_cons
                                                                                                         (__toplevel_cons 'op (__toplevel_cons '+ ()))
                                                                                                         (__toplevel_cons
                                                                                                            (__toplevel_cons 'reg (__toplevel_cons 'val ()))
                                                                                                            (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'n ())) ())))))
                                                                                                (__toplevel_cons
                                                                                                   (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'continue ())) ()))
                                                                                                   (__toplevel_cons
                                                                                                      'immediate-answer
                                                                                                      (__toplevel_cons
                                                                                                         (__toplevel_cons
                                                                                                            'assign
                                                                                                            (__toplevel_cons 'val (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'n ())) ())))
                                                                                                         (__toplevel_cons
                                                                                                            (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'continue ())) ()))
                                                                                                            (__toplevel_cons 'fib-done ())))))))))))))))))))))))))))))))
         ((lambda (x) x) (display "(fib 5): "))
         (start fib-machine)
         (display (get-register-contents fib-machine 'val))
         (newline)))
   (<change>
      (let ((fib-machine (make-machine
                           (__toplevel_cons 'continue (__toplevel_cons 'n (__toplevel_cons 'val ())))
                           ops
                           (__toplevel_cons
                              'start
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'assign
                                    (__toplevel_cons
                                       'continue
                                       (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'fib-done ())) ())))
                                 (__toplevel_cons
                                    'fib-loop
                                    (__toplevel_cons
                                       (__toplevel_cons
                                          'test
                                          (__toplevel_cons
                                             (__toplevel_cons 'op (__toplevel_cons '< ()))
                                             (__toplevel_cons
                                                (__toplevel_cons 'reg (__toplevel_cons 'n ()))
                                                (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 2 ())) ()))))
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'branch
                                             (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'immediate-answer ())) ()))
                                          (__toplevel_cons
                                             (__toplevel_cons 'save (__toplevel_cons 'continue ()))
                                             (__toplevel_cons
                                                (__toplevel_cons 'save (__toplevel_cons 'n ()))
                                                (__toplevel_cons
                                                   (__toplevel_cons
                                                      'assign
                                                      (__toplevel_cons
                                                         'continue
                                                         (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'afterfib-n-1 ())) ())))
                                                   (__toplevel_cons
                                                      (__toplevel_cons
                                                         'assign
                                                         (__toplevel_cons
                                                            'n
                                                            (__toplevel_cons
                                                               (__toplevel_cons 'op (__toplevel_cons '- ()))
                                                               (__toplevel_cons
                                                                  (__toplevel_cons 'reg (__toplevel_cons 'n ()))
                                                                  (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 1 ())) ())))))
                                                      (__toplevel_cons
                                                         (__toplevel_cons
                                                            'goto
                                                            (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'fib-loop ())) ()))
                                                         (__toplevel_cons
                                                            'afterfib-n-1
                                                            (__toplevel_cons
                                                               (__toplevel_cons 'restore (__toplevel_cons 'n ()))
                                                               (__toplevel_cons
                                                                  (__toplevel_cons 'restore (__toplevel_cons 'continue ()))
                                                                  (__toplevel_cons
                                                                     (__toplevel_cons
                                                                        'assign
                                                                        (__toplevel_cons
                                                                           'n
                                                                           (__toplevel_cons
                                                                              (__toplevel_cons 'op (__toplevel_cons '- ()))
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons 'reg (__toplevel_cons 'n ()))
                                                                                 (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 2 ())) ())))))
                                                                     (__toplevel_cons
                                                                        (__toplevel_cons 'save (__toplevel_cons 'continue ()))
                                                                        (__toplevel_cons
                                                                           (__toplevel_cons
                                                                              'assign
                                                                              (__toplevel_cons
                                                                                 'continue
                                                                                 (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'afterfib-n-2 ())) ())))
                                                                           (__toplevel_cons
                                                                              (__toplevel_cons 'save (__toplevel_cons 'val ()))
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons
                                                                                    'goto
                                                                                    (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'fib-loop ())) ()))
                                                                                 (__toplevel_cons
                                                                                    'afterfib-n-2
                                                                                    (__toplevel_cons
                                                                                       (__toplevel_cons
                                                                                          'assign
                                                                                          (__toplevel_cons 'n (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ())))
                                                                                       (__toplevel_cons
                                                                                          (__toplevel_cons 'restore (__toplevel_cons 'val ()))
                                                                                          (__toplevel_cons
                                                                                             (__toplevel_cons 'restore (__toplevel_cons 'continue ()))
                                                                                             (__toplevel_cons
                                                                                                (__toplevel_cons
                                                                                                   'assign
                                                                                                   (__toplevel_cons
                                                                                                      'val
                                                                                                      (__toplevel_cons
                                                                                                         (__toplevel_cons 'op (__toplevel_cons '+ ()))
                                                                                                         (__toplevel_cons
                                                                                                            (__toplevel_cons 'reg (__toplevel_cons 'val ()))
                                                                                                            (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'n ())) ())))))
                                                                                                (__toplevel_cons
                                                                                                   (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'continue ())) ()))
                                                                                                   (__toplevel_cons
                                                                                                      'immediate-answer
                                                                                                      (__toplevel_cons
                                                                                                         (__toplevel_cons
                                                                                                            'assign
                                                                                                            (__toplevel_cons 'val (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'n ())) ())))
                                                                                                         (__toplevel_cons
                                                                                                            (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'continue ())) ()))
                                                                                                            (__toplevel_cons 'fib-done ())))))))))))))))))))))))))))))))
         (display "(fib 5): ")
         (set-register-contents! fib-machine 'n 5)
         (start fib-machine)
         (display (get-register-contents fib-machine 'val))
         (newline))
      (let ((fac-machine (make-machine
                           (__toplevel_cons 'continue (__toplevel_cons 'n (__toplevel_cons 'val ())))
                           ops
                           (__toplevel_cons
                              'start
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'assign
                                    (__toplevel_cons
                                       'continue
                                       (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'fact-done ())) ())))
                                 (__toplevel_cons
                                    'fact-loop
                                    (__toplevel_cons
                                       (__toplevel_cons
                                          'test
                                          (__toplevel_cons
                                             (__toplevel_cons 'op (__toplevel_cons '= ()))
                                             (__toplevel_cons
                                                (__toplevel_cons 'reg (__toplevel_cons 'n ()))
                                                (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 1 ())) ()))))
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'branch
                                             (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'base-case ())) ()))
                                          (__toplevel_cons
                                             (__toplevel_cons 'save (__toplevel_cons 'continue ()))
                                             (__toplevel_cons
                                                (__toplevel_cons 'save (__toplevel_cons 'n ()))
                                                (__toplevel_cons
                                                   (__toplevel_cons
                                                      'assign
                                                      (__toplevel_cons
                                                         'n
                                                         (__toplevel_cons
                                                            (__toplevel_cons 'op (__toplevel_cons '- ()))
                                                            (__toplevel_cons
                                                               (__toplevel_cons 'reg (__toplevel_cons 'n ()))
                                                               (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 1 ())) ())))))
                                                   (__toplevel_cons
                                                      (__toplevel_cons
                                                         'assign
                                                         (__toplevel_cons
                                                            'continue
                                                            (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'after-fact ())) ())))
                                                      (__toplevel_cons
                                                         (__toplevel_cons
                                                            'goto
                                                            (__toplevel_cons (__toplevel_cons 'label (__toplevel_cons 'fact-loop ())) ()))
                                                         (__toplevel_cons
                                                            'after-fact
                                                            (__toplevel_cons
                                                               (__toplevel_cons 'restore (__toplevel_cons 'n ()))
                                                               (__toplevel_cons
                                                                  (__toplevel_cons 'restore (__toplevel_cons 'continue ()))
                                                                  (__toplevel_cons
                                                                     (__toplevel_cons
                                                                        'assign
                                                                        (__toplevel_cons
                                                                           'val
                                                                           (__toplevel_cons
                                                                              (__toplevel_cons 'op (__toplevel_cons '* ()))
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons 'reg (__toplevel_cons 'n ()))
                                                                                 (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'val ())) ())))))
                                                                     (__toplevel_cons
                                                                        (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'continue ())) ()))
                                                                        (__toplevel_cons
                                                                           'base-case
                                                                           (__toplevel_cons
                                                                              (__toplevel_cons
                                                                                 'assign
                                                                                 (__toplevel_cons 'val (__toplevel_cons (__toplevel_cons 'const (__toplevel_cons 1 ())) ())))
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons 'goto (__toplevel_cons (__toplevel_cons 'reg (__toplevel_cons 'continue ())) ()))
                                                                                 (__toplevel_cons 'fact-done ()))))))))))))))))))))))
         (display "(fac 5): ")
         (display (get-register-contents fac-machine 'val))
         (start fac-machine)
         ((lambda (x) x) (newline)))))