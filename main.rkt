#lang racket/base

(require racket/system racket/string racket/file)

(provide (all-defined-out))

(struct gum-exception exn:fail:user ())
(struct gum-SIGINT gum-exception ())

(define choose: "choose")
(define confirm: "confirm")
(define file: "file")
(define filter: "filter")
(define format: "format")
(define input: "input")
(define join: "join")
(define pager: "pager")
(define spin: "spin")
(define style: "style")
(define table: "table")
(define write: "write")
(define log: "log")

(define (gum:)
  (list choose:
        confirm:
        file:
        filter:
        format:
        input:
        join:
        pager:
        spin:
        style:
        table:
        write:
        log:))

(define (gum-kw-args-rest kws kw-args command . rest)
  (run
   (let* ([format-rest (lambda (v)
                        (cond [(or (string? v) (number? v)) (format "\"~a\"" v)]
                              [(boolean? v) (if v "true" "false")]
                              [else (error v)]))]
          [formatkv    (lambda (k-v) (format "--~a=~a" (car k-v) (format-rest (cdr k-v))))])
     (append (list command)
             (map formatkv (for/list ([k kws] [v kw-args]) (cons (keyword->string k) v)))
             (map format-rest rest)))))

(define gum (make-keyword-procedure gum-kw-args-rest))


(define (run gum-cmd-list)
  (let* ([fout          (make-temporary-file)]
         [command       (car gum-cmd-list)]
         [cmd           (string-join gum-cmd-list)]
         [system/cmd    (format "gum ~a  1> ~a"  cmd  (path->string fout))]
         [code          (system/exit-code system/cmd)]
         [stdout        (string-trim (file->string fout) #:left? #f)]
         [stdout/nil?   (equal? "" stdout)]
         [command/code? (lambda (cmd return) (and (equal? command cmd) (= return code)))])
    (cond
      [(command/code? confirm: 0) #t]
      [(and (command/code? confirm: 1) stdout/nil?) #f]
      [(command/code? file: 0) (build-path stdout)]
      [(command/code? choose: 0) (string-split stdout "\n")]
      [(command/code? filter: 0) (string-split stdout "\n")]
      [(= code 0)   stdout]
      [(= code 130) (raise (gum-SIGINT (format "SIGINT: ~a " system/cmd) (current-continuation-marks)))]
      [else         (raise (gum-exception (format "~a~n~a" system/cmd stdout) (current-continuation-marks)))])))



(module+ test
  (gum choose: "Option 1" "Option 2" "Option 3")
  (gum confirm:"Are you sure?")
  (gum file:  "/path/to/folder")
  (gum filter: "Option 1" "Option 2" "Option 3")
  (gum format: "# Markdown title \n * list")
  (gum input: #:placeholder "ingrese texto")
  (gum join: "Line 1" "Line 2" "Line 3")
  (gum pager: "Content to scroll through")
  (gum spin: "sleep 10")
  (gum join:
    (gum style:
          #:foreground 212 #:border-foreground 212 #:border "double"
          #:align "center" #:width 50 #:margin "1 2" #:padding "2 4"
          "Title" "New Body")
    (gum style:
          #:foreground 212 #:border-foreground 212 #:border "double"
          #:align "center" #:width 50 #:margin "1 2" #:padding "2 4"
          "Bubble Gum (1¢)" "So sweet and so fresh!"))
  (gum write:)
  (gum log: "Log message" #:level "debug")

)