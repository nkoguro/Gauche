(use gauche.test)
(use gauche.uvector)
(use lang.asm.object)

(test-start "lang.asm.x86_64")

(use lang.asm.x86_64)
(test-module 'lang.asm.x86_64)

;;----------------------------------------------------------------------
(test-section "instructions")

;; Helper: assemble and return (bytes . label-alist) for comparison.
;; Label alist is sorted by address for deterministic comparison.
;; bytes is compared as a list for readability.
(define (t-asm name insns expected-bytes expected-labels)
  (test* name
         (cons expected-bytes expected-labels)
         (receive (bytes labels) (asm insns)
           (cons (u8vector->list bytes)
                 (sort labels (^[a b] (< (cdr a) (cdr b))))))))

;; --- Trivial single-byte instructions ---

(t-asm "ret"     '((ret))     '(#xc3) '())
(t-asm "int3"    '((int3))    '(#xcc) '())
(t-asm "endbr64" '((endbr64)) '(#xf3 #x0f #x1e #xfa) '())

;; --- push / pop (low registers: no REX prefix) ---

(t-asm "push %rax" '((push %rax)) '(#x50) '())
(t-asm "push %rcx" '((push %rcx)) '(#x51) '())
(t-asm "push %rbp" '((push %rbp)) '(#x55) '())
(t-asm "push %rdi" '((push %rdi)) '(#x57) '())
(t-asm "pop  %rbp" '((pop  %rbp)) '(#x5d) '())
(t-asm "pop  %rbx" '((pop  %rbx)) '(#x5b) '())

;; --- push / pop (extended registers %r8-%r15: need REX.B = #x41) ---

(t-asm "push %r8"  '((push %r8))  '(#x41 #x50) '())
(t-asm "push %r15" '((push %r15)) '(#x41 #x57) '())
(t-asm "pop  %r12" '((pop  %r12)) '(#x41 #x5c) '())
(t-asm "pop  %r13" '((pop  %r13)) '(#x41 #x5d) '())

;; --- call / ret ---

;; call via register: FF /2 ModRM(11 010 reg)
(t-asm "call %rax" '((call %rax)) '(#xff #xd0) '())
(t-asm "call %rdx" '((call %rdx)) '(#xff #xd2) '())
;; call extended register: 41 FF /2
(t-asm "call %r9"  '((call %r9))  '(#x41 #xff #xd1) '())

;; --- movb (8-bit moves) ---

;; movb reg8→reg8: 88 /r
(t-asm "movb %al,%cl"  '((movb %al %cl))  '(#x88 #xc1) '())
(t-asm "movb %dl,%bl"  '((movb %dl %bl))  '(#x88 #xd3) '())

;; movb mem→reg8: 8A /r
(t-asm "movb (%rax),%dl" '((movb (%rax) %dl)) '(#x8a #x10) '())

;; movb imm8→reg8: B0+rd ib
(t-asm "movb $42,%bl"  '((movb 42 %bl))  '(#xb3 #x2a) '())
(t-asm "movb $0,%al"   '((movb 0 %al))   '(#xb0 #x00) '())

;; movb imm8→mem: C6 /0 ib
(t-asm "movb $42,(%rax)" '((movb 42 (%rax))) '(#xc6 #x00 #x2a) '())

;; --- movq (64-bit moves) ---

;; movq reg→reg: REX.W 89 /r  ModRM(11 src dst)
(t-asm "movq %rax,%rcx" '((movq %rax %rcx)) '(#x48 #x89 #xc1) '())
(t-asm "movq %rsp,%rbp" '((movq %rsp %rbp)) '(#x48 #x89 #xe5) '())
(t-asm "movq %rax,%rax" '((movq %rax %rax)) '(#x48 #x89 #xc0) '())

;; movq extended registers: REX.W + REX.R/B as needed
;; movq %r9,%r11: 4D 89 CB  (REX.W+R+B, 89, ModRM 11_001_011)
(t-asm "movq %r9,%r11"  '((movq %r9  %r11)) '(#x4d #x89 #xcb) '())
;; movq %rax,%r10: 49 89 C2  (REX.W+B, 89, ModRM 11_000_010)
(t-asm "movq %rax,%r10" '((movq %rax %r10)) '(#x49 #x89 #xc2) '())
;; movq %r8,%rcx: 4C 89 C1  (REX.W+R, 89, ModRM 11_000_001)
(t-asm "movq %r8,%rcx"  '((movq %r8  %rcx)) '(#x4c #x89 #xc1) '())

;; movq reg→mem: REX.W 89 /r
(t-asm "movq %rax,(%rax)"       '((movq %rax (%rax)))      '(#x48 #x89 #x00) '())
;; movq reg→mem with disp8 and non-SIB base: 48 89 43 10
(t-asm "movq %rax,16(%rbx)"     '((movq %rax (16 %rbx)))   '(#x48 #x89 #x43 #x10) '())
;; movq reg→neg-disp8(%rbp): 48 89 45 F8
(t-asm "movq %rax,-8(%rbp)"     '((movq %rax (-8 %rbp)))   '(#x48 #x89 #x45 #xf8) '())

;; movq mem→reg: REX.W 8B /r
(t-asm "movq (%rax),%rcx"       '((movq (%rax) %rcx))      '(#x48 #x8b #x08) '())
;; movq base+disp8 → reg: 48 8B 45 08
(t-asm "movq 8(%rbp),%rax"      '((movq (8 %rbp) %rax))    '(#x48 #x8b #x45 #x08) '())
;; movq (%rsp) needs SIB: 48 8B 04 24
(t-asm "movq (%rsp),%rax"       '((movq (%rsp) %rax))      '(#x48 #x8b #x04 #x24) '())
;; movq (%rbp) needs disp8=0: 48 8B 45 00
(t-asm "movq (%rbp),%rax"       '((movq (%rbp) %rax))      '(#x48 #x8b #x45 #x00) '())
;; movq with extended base: 4D 89 01  (movq %r8,(%r9))
(t-asm "movq %r8,(%r9)"         '((movq %r8 (%r9)))         '(#x4d #x89 #x01) '())

;; movq imm8→reg: REX.W C7 /0 id  (sign-extended 32-bit immediate)
(t-asm "movq $42,%rax"  '((movq 42 %rax))  '(#x48 #xc7 #xc0 #x2a #x00 #x00 #x00) '())
(t-asm "movq $0,%rbx"   '((movq 0 %rbx))   '(#x48 #xc7 #xc3 #x00 #x00 #x00 #x00) '())

;; movq imm32→reg: REX.W C7 /0 id
(t-asm "movq $1000,%rbx" '((movq 1000 %rbx))
       '(#x48 #xc7 #xc3 #xe8 #x03 #x00 #x00) '())

;; movq imm64→reg: REX.W B8+rd iq
(t-asm "movq $imm64,%rax"
       '((movq (imm64 #x0102030405060708) %rax))
       '(#x48 #xb8 #x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01) '())
;; movq imm64 to extended reg: REX.W+B B8+2 iq  (%r10)
(t-asm "movq $imm64,%r10"
       '((movq (imm64 #x0102030405060708) %r10))
       '(#x49 #xba #x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01) '())

;; movzbq / movzwq (zero-extend): REX.W 0F B6/B7 /r
(t-asm "movzbq (%rsi),%rdi" '((movzbq (%rsi) %rdi)) '(#x48 #x0f #xb6 #x3e) '())
(t-asm "movzwq (%rax),%rcx" '((movzwq (%rax) %rcx)) '(#x48 #x0f #xb7 #x08) '())

;; leaq: REX.W 8D /r
(t-asm "leaq 8(%rbx),%rax"  '((leaq (8 %rbx) %rax))  '(#x48 #x8d #x43 #x08) '())
(t-asm "leaq -16(%rbp),%rsi" '((leaq (-16 %rbp) %rsi)) '(#x48 #x8d #x75 #xf0) '())

;; --- SIB addressing ---

;; movq (%rax,%rcx,4),%rdx: 48 8B 14 88
(t-asm "movq (%rax,%rcx,4),%rdx"
       '((movq (%rax %rcx 4) %rdx)) '(#x48 #x8b #x14 #x88) '())
;; movq 8(%rax,%rcx,4),%rdx: 48 8B 54 88 08
(t-asm "movq 8(%rax,%rcx,4),%rdx"
       '((movq (8 %rax %rcx 4) %rdx)) '(#x48 #x8b #x54 #x88 #x08) '())

;; --- ALU instructions (addq/subq/andq/orq/xorq/cmpq) ---

;; addq reg→reg: REX.W 01 /r  ModRM(11 src dst)
(t-asm "addq %rcx,%rax" '((addq %rcx %rax)) '(#x48 #x01 #xc8) '())
;; addq imm8→reg: REX.W 83 /0 ib
(t-asm "addq $100,%rbx" '((addq 100 %rbx)) '(#x48 #x83 #xc3 #x64) '())
;; addq imm32→%rax uses short form: REX.W 05 id
(t-asm "addq $1000,%rax" '((addq 1000 %rax)) '(#x48 #x05 #xe8 #x03 #x00 #x00) '())
;; addq imm32→non-rax: REX.W 81 /0 id
(t-asm "addq $1000,%rbx" '((addq 1000 %rbx)) '(#x48 #x81 #xc3 #xe8 #x03 #x00 #x00) '())

;; subq reg→reg: REX.W 29 /r
(t-asm "subq %rcx,%rax"  '((subq %rcx %rax))  '(#x48 #x29 #xc8) '())
;; subq imm8→reg: REX.W 83 /5 ib
(t-asm "subq $16,%rsp"   '((subq 16 %rsp))    '(#x48 #x83 #xec #x10) '())
;; subq reg→mem (%rsp via SIB): REX.W 29 /r
(t-asm "subq %rcx,(%rsp)" '((subq %rcx (%rsp))) '(#x48 #x29 #x0c #x24) '())

;; orq: REX.W 09 /r
(t-asm "orq %rdx,%rsi"   '((orq %rdx %rsi))   '(#x48 #x09 #xd6) '())

;; andq with extended src: REX.W+R 21 /r
(t-asm "andq %r8,%rcx"   '((andq %r8 %rcx))   '(#x4c #x21 #xc1) '())

;; xorq reg→reg: REX.W 31 /r  (canonical zero-register idiom)
(t-asm "xorq %rax,%rax"  '((xorq %rax %rax))  '(#x48 #x31 #xc0) '())

;; cmpq imm8→reg: REX.W 83 /7 ib
(t-asm "cmpq $0,%rax"    '((cmpq 0 %rax))     '(#x48 #x83 #xf8 #x00) '())
;; cmpq imm32→%rax short form: REX.W 3D id
(t-asm "cmpq $256,%rax"  '((cmpq 256 %rax))   '(#x48 #x3d #x00 #x01 #x00 #x00) '())
;; cmpq reg→reg: REX.W 39 /r
(t-asm "cmpq %rbx,%rcx"  '((cmpq %rbx %rcx))  '(#x48 #x39 #xd9) '())

;; --- Shift / rotate ---

;; shl by-1 form: REX.W D1 /4
(t-asm "shl $1,%rax"  '((shl 1 %rax))  '(#x48 #xd1 #xe0) '())
;; shl by-imm8 form: REX.W C1 /4 ib
(t-asm "shl $2,%rcx"  '((shl 2 %rcx))  '(#x48 #xc1 #xe1 #x02) '())
;; shr by-1: REX.W D1 /5
(t-asm "shr $1,%rax"  '((shr 1 %rax))  '(#x48 #xd1 #xe8) '())
;; sar by-imm8: REX.W C1 /7 ib
(t-asm "sar $3,%rdx"  '((sar 3 %rdx))  '(#x48 #xc1 #xfa #x03) '())
;; rol by-imm8: REX.W C1 /0 ib
(t-asm "rol $3,%rax"  '((rol 3 %rax))  '(#x48 #xc1 #xc0 #x03) '())
;; ror by-1: REX.W D1 /1
(t-asm "ror $1,%rcx"  '((ror 1 %rcx))  '(#x48 #xd1 #xc9) '())

;; --- incq / decq ---

;; incq: REX.W FF /0  ModRM(11 000 reg)
(t-asm "incq %rax"   '((incq %rax))   '(#x48 #xff #xc0) '())
(t-asm "incq %rdx"   '((incq %rdx))   '(#x48 #xff #xc2) '())
;; decq: REX.W FF /1  ModRM(11 001 reg)
(t-asm "decq %rax"   '((decq %rax))   '(#x48 #xff #xc8) '())
(t-asm "decq %rcx"   '((decq %rcx))   '(#x48 #xff #xc9) '())
;; incq mem: REX.W FF /0 with memory operand
(t-asm "incq (%rdi)" '((incq (%rdi))) '(#x48 #xff #x07) '())

;; --- Embedded data directives ---

;; .datab: emit raw bytes
(t-asm ".datab 4 bytes"
       '((.datab (imm8 #xde) (imm8 #xad) (imm8 #xbe) (imm8 #xef)))
       '(#xde #xad #xbe #xef) '())

;; .datal: emit 32-bit little-endian value
(t-asm ".datal"
       '((.datal (imm32 #x12345678)))
       '(#x78 #x56 #x34 #x12) '())

;; .dataq: emit 64-bit little-endian value
(t-asm ".dataq"
       '((.dataq (imm64 #x0102030405060708)))
       '(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01) '())

;; --- Labels: backward reference ---

;; A short backward jump: EB rel8 where rel8 = -(size of loop body + 2)
;; layout: loop[0] incq(3) jmp[3](2) end[5]
;; jmp offset = 0 - 5 = -5 = #xfb
(t-asm "backward jmp"
       '(loop:
         (incq %rax)    ; 3 bytes  [addr 0..2]
         (jmp loop:))   ; 2 bytes  [addr 3..4], end addr=5
       '(#x48 #xff #xc0 #xeb #xfb)
       '((loop: . 0)))

;; --- Labels: forward reference ---

;; A short forward conditional jump: 75 rel8
;; layout: jne[0](2) movq[2](3) skip[5]
;; jne offset = 5 - 2 = 3
(t-asm "forward jne"
       '((jne skip:)      ; 2 bytes  [addr 0..1], end addr=2
         (movq %rax %rax) ; 3 bytes  [addr 2..4], end addr=5
         skip:)
       '(#x75 #x03 #x48 #x89 #xc0)
       '((skip: . 5)))

;; --- Labels: long (32-bit) conditional jump ---

;; jel (je long): 0F 84 rel32
;; layout: jel[0](6) movq[6](3) far[9]
;; jel offset = 9 - 6 = 3
(t-asm "forward jel (long je)"
       '((jel far:)       ; 6 bytes  [addr 0..5], end addr=6
         (movq %rax %rax) ; 3 bytes  [addr 6..8], end addr=9
         far:)
       '(#x0f #x84 #x03 #x00 #x00 #x00 #x48 #x89 #xc0)
       '((far: . 9)))

;; --- Labels: long unconditional jump ---

;; jmpl: E9 rel32
;; layout: jmpl[0](5) movq[5](3) far[8]
;; offset = 8 - 5 = 3
(t-asm "forward jmpl (long jmp)"
       '((jmpl far:)      ; 5 bytes  [addr 0..4], end addr=5
         (movq %rax %rax) ; 3 bytes  [addr 5..7], end addr=8
         far:)
       '(#xe9 #x03 #x00 #x00 #x00 #x48 #x89 #xc0)
       '((far: . 8)))

;; --- Labels: multiple labels, backward and forward ---

;; Typical function prologue/loop/epilogue:
;; entry[0] push(1) movq(3) loop[4] decq(3) jne(2) pop[9](1) ret[10](1)
;; jne loop: offset = 4 - 9 = -5 = #xfb
(t-asm "multiple labels"
       '(entry:
         (push %rbp)       ; 1 byte   [addr 0], end=1
         (movq %rsp %rbp)  ; 3 bytes  [addr 1..3], end=4
         loop:
         (decq %rcx)       ; 3 bytes  [addr 4..6], end=7
         (jne loop:)       ; 2 bytes  [addr 7..8], end=9
         (pop %rbp)        ; 1 byte   [addr 9], end=10
         (ret))            ; 1 byte   [addr 10], end=11
       '(#x55 #x48 #x89 #xe5 #x48 #xff #xc9 #x75 #xfb #x5d #xc3)
       '((entry: . 0) (loop: . 4)))

;; --- Labels: RIP-relative memory access ---

;; movq data(%rip),%rax  then the data label immediately follows.
;; movq is 7 bytes [addr 0..6], end addr=7; data label at addr=7.
;; RIP-relative offset = 7 - 7 = 0  → disp32 = 0
(t-asm "rip-relative load"
       '((movq (data: %rip) %rax) ; 7 bytes [addr 0..6], end=7
         data:
         (.dataq (imm64 42)))     ; 8 bytes [addr 7..14]
       '(#x48 #x8b #x05 #x00 #x00 #x00 #x00
         #x2a #x00 #x00 #x00 #x00 #x00 #x00 #x00)
       '((data: . 7)))

;; --- asm-template / link-template round-trip (no placeholders) ---

(test-section "templates")

(let ()
  (define tmpl (asm-template '((movq %rax %rcx))))
  (test* "asm-template type" #t (is-a? tmpl <obj-template>))
  (receive (bytes labels) (link-template tmpl '())
    (test* "asm-template round-trip bytes"  '(#x48 #x89 #xc1) (u8vector->list bytes))
    (test* "asm-template round-trip labels" '() labels)))

(let ()
  (define tmpl (asm-template '(entry:
                                (push %rbp)
                                (movq %rsp %rbp)
                                (ret))))
  (receive (bytes labels) (link-template tmpl '())
    (test* "asm-template with labels bytes"
           '(#x55 #x48 #x89 #xe5 #xc3)
           (u8vector->list bytes))
    (test* "asm-template with labels alist"
           '((entry: . 0))
           labels)))

;; Verify that link-template does not mutate the template's byte vector
;; (same template can be instantiated multiple times).
(let ()
  (define tmpl (asm-template '((ret))))
  (receive (b1 _) (link-template tmpl '())
    (receive (b2 _) (link-template tmpl '())
      (test* "link-template returns fresh vector"
             #f
             (eq? b1 b2)))))

;; --- immediate-value placeholders ---

;; imm64 placeholder: movq (imm64 :val), %rax
;; Encoding: REX.W(48) B8 <8 bytes immediate>  -- 10 bytes total
(let ()
  (define tmpl (asm-template '((movq (imm64 :val) %rax))))
  ;; Default instantiation (zero)
  (receive (b _) (link-template tmpl '())
    (test* "imm64 placeholder default"
           '(#x48 #xb8 0 0 0 0 0 0 0 0)
           (u8vector->list b)))
  ;; Instantiate with a real 64-bit value
  (receive (b _) (link-template tmpl `((:val ,<uint64> #x0102030405060708)))
    (test* "imm64 placeholder filled"
           '(#x48 #xb8 #x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01)
           (u8vector->list b)))
  ;; Re-instantiate with a different value to confirm immutability of template
  (receive (b _) (link-template tmpl `((:val ,<uint64> 1)))
    (test* "imm64 placeholder re-instantiate"
           '(#x48 #xb8 #x01 0 0 0 0 0 0 0)
           (u8vector->list b))))

;; imm64 placeholder into an extended register: movq (imm64 :v), %r10
;; Encoding: REX.W+B(49) BA <8 bytes> -- 10 bytes
(let ()
  (define tmpl (asm-template '((movq (imm64 :v) %r10))))
  (receive (b _) (link-template tmpl `((:v ,<uint64> #x0102030405060708)))
    (test* "imm64 placeholder %r10"
           '(#x49 #xba #x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01)
           (u8vector->list b))))

;; imm32 placeholder: movq (imm32 :x), %rbx
;; Encoding: REX.W(48) C7 /0 ModRM(C3) <4 bytes> -- 7 bytes total
(let ()
  (define tmpl (asm-template '((movq (imm32 :x) %rbx))))
  (receive (b _) (link-template tmpl '())
    (test* "imm32 placeholder default"
           '(#x48 #xc7 #xc3 0 0 0 0)
           (u8vector->list b)))
  (receive (b _) (link-template tmpl `((:x ,<uint32> 1000)))
    (test* "imm32 placeholder filled"
           '(#x48 #xc7 #xc3 #xe8 #x03 #x00 #x00)
           (u8vector->list b))))

;; imm8 placeholder: movb (imm8 :n), %al
;; Encoding: B0 <1 byte> -- 2 bytes total
(let ()
  (define tmpl (asm-template '((movb (imm8 :n) %al))))
  (receive (b _) (link-template tmpl '())
    (test* "imm8 placeholder default"
           '(#xb0 0)
           (u8vector->list b)))
  (receive (b _) (link-template tmpl `((:n ,<uint8> 42)))
    (test* "imm8 placeholder filled"
           '(#xb0 #x2a)
           (u8vector->list b))))

;; Multiple placeholders in one template, including a non-placeholder instruction
(let ()
  (define tmpl (asm-template '((movq (imm64 :fn) %rax)
                               (movb (imm8  :nb) %cl)
                               (ret))))
  (receive (b _) (link-template tmpl `((:fn ,<uint64> #xdeadbeef00112233)
                                       (:nb ,<uint8> 7)))
    (test* "multiple placeholders"
           (append '(#x48 #xb8 #x33 #x22 #x11 #x00 #xef #xbe #xad #xde)
                   '(#xb1 #x07)
                   '(#xc3))
           (u8vector->list b)))
  ;; Partial instantiation: only :fn supplied, :nb stays 0
  (receive (b _) (link-template tmpl `((:fn ,<uint64> 1)))
    (test* "partial instantiation"
           (append '(#x48 #xb8 #x01 0 0 0 0 0 0 0)
                   '(#xb1 0)
                   '(#xc3))
           (u8vector->list b))))

;; Placeholder can appear multiple times
(let ()
  (define tmpl (asm-template '((movq (imm64 :a) %rax)
                               (movb (imm8  :b) %cl)
                               (movq (imm64 :a) %rax)
                               (movb (imm8  :b) %cl)
                               (ret))))
  (receive (b _) (link-template tmpl `((:a ,<uint64> #xcafebabe01234567)
                                       (:b ,<uint8> #xfe)))
    (test* "placeholders appear multiple times"
           (append '(#x48 #xb8 #x67 #x45 #x23 #x01 #xbe #xba #xfe #xca)
                   '(#xb1 #xfe)
                   '(#x48 #xb8 #x67 #x45 #x23 #x01 #xbe #xba #xfe #xca)
                   '(#xb1 #x0fe)
                   '(#xc3))
           (u8vector->list b))))

;; Placeholder in a template that also has labels
(let ()
  (define tmpl (asm-template '(start:
                               (movq (imm64 :ptr) %rax)
                               (ret))))
  (receive (b labels) (link-template tmpl `((:ptr ,<uint64> #xff)))
    (test* "placeholder with label bytes"
           '(#x48 #xb8 #xff 0 0 0 0 0 0 0 #xc3)
           (u8vector->list b))
    (test* "placeholder with label labels"
           '((start: . 0))
           labels)))

;; --- data-directive placeholders ---

;; .dataq placeholder: 8-byte hole
(let ()
  (define tmpl (asm-template '((.dataq :addr))))
  (receive (b _) (link-template tmpl '())
    (test* ".dataq placeholder default"
           '(0 0 0 0 0 0 0 0)
           (u8vector->list b)))
  (receive (b _) (link-template tmpl `((:addr ,<uint64> #x0102030405060708)))
    (test* ".dataq placeholder filled"
           '(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01)
           (u8vector->list b))))

;; .datal placeholder: 4-byte hole
(let ()
  (define tmpl (asm-template '((.datal :v))))
  (receive (b _) (link-template tmpl `((:v ,<uint32> #xdeadbeef)))
    (test* ".datal placeholder filled"
           '(#xef #xbe #xad #xde)
           (u8vector->list b))))

;; .datab placeholder: 1-byte hole
(let ()
  (define tmpl (asm-template '((.datab :b))))
  (receive (b _) (link-template tmpl '())
    (test* ".datab placeholder default" '(0) (u8vector->list b)))
  (receive (b _) (link-template tmpl `((:b ,<uint8> #xa5)))
    (test* ".datab placeholder filled" '(#xa5) (u8vector->list b))))

;; .datal placeholder with <float>
(let ()
  (define tmpl (asm-template '((.datal :x))))
  (receive (b _) (link-template tmpl `((:x ,<float> 1.0)))
    (test* ".datal <float> 1.0"
           '(#x00 #x00 #x80 #x3f)
           (u8vector->list b)))
  (receive (b _) (link-template tmpl `((:x ,<float> -2.5)))
    (test* ".datal <float> -2.5"
           '(#x00 #x00 #x20 #xc0)
           (u8vector->list b))))

;; .dataq placeholder with <float> and <double>
(let ()
  (define tmpl (asm-template '((.dataq :x))))
  (receive (b _) (link-template tmpl `((:x ,<float> 1.0)))
    (test* ".datal <float> 1.0"
           '(#x00 #x00 #x80 #x3f 0 0 0 0)
           (u8vector->list b)))
  (receive (b _) (link-template tmpl `((:x ,<float> -2.5)))
    (test* ".datal <float> -2.5"
           '(#x00 #x00 #x20 #xc0 0 0 0 0)
           (u8vector->list b)))
  (receive (b _) (link-template tmpl `((:x ,<double> 1.0)))
    (test* ".dataq <double> 1.0"
           '(#x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x3f)
           (u8vector->list b)))
  (receive (b _) (link-template tmpl `((:x ,<double> 3.14)))
    (test* ".dataq <double> 3.14"
           '(#x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40)
           (u8vector->list b))))

;; Mix: placeholder data following a real instruction
(let ()
  (define tmpl (asm-template '((ret) (.datal :v))))
  (receive (b _) (link-template tmpl `((:v ,<uint32> #xdeadbeef)))
    (test* ".datal placeholder after ret"
           '(#xc3 #xef #xbe #xad #xde)
           (u8vector->list b))))

;; Mix: instruction placeholder and data placeholder sharing one keyword
(let ()
  (define tmpl (asm-template '((.dataq :fn-ptr)
                                (movq (imm64 :fn-ptr) %rax)
                                (ret))))
  (receive (b _) (link-template tmpl `((:fn-ptr ,<uint64> #x0102030405060708)))
    (test* "data and imm64 placeholder same keyword"
           (append '(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01) ; .dataq
                   '(#x48 #xb8 #x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01) ; movq
                   '(#xc3))                                               ; ret
           (u8vector->list b))))

;; Existing literal data forms still work unchanged
(let ()
  (define tmpl (asm-template '((.dataq (imm64 #x0102030405060708)))))
  (receive (b _) (link-template tmpl '())
    (test* ".dataq literal unchanged"
           '(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01)
           (u8vector->list b))))

;; --- movs_ instruction-variant placeholder ---

;; ((movs_ :op) %xmm0 %xmm1)  -- sse->sse form
;; Default encoding: F2 0F 10 ModRM
;; ModRM: mod=3(reg-reg), reg=1(xmm1/dst), r/m=1(xmm1/dst) -> 0b11_001_001 = #xc9
(let ()
  (define tmpl (asm-template '(((movs_ :op) %xmm0 %xmm1))))
  ;; Default is movsd
  (receive (b _) (link-template tmpl '())
    (test* "movs_ sse->sse default (movsd)"
           '(#xf2 #x0f #x10 #xc9)
           (u8vector->list b)))
  ;; Explicit movsd
  (receive (b _) (link-template tmpl '((:op #f movsd)))
    (test* "movs_ sse->sse explicit movsd"
           '(#xf2 #x0f #x10 #xc9)
           (u8vector->list b)))
  ;; Switch to movss
  (receive (b _) (link-template tmpl '((:op #f movss)))
    (test* "movs_ sse->sse switched to movss"
           '(#xf3 #x0f #x10 #xc9)
           (u8vector->list b)))
  ;; Re-instantiate as movsd again (template not mutated)
  (receive (b _) (link-template tmpl '((:op #f movsd)))
    (test* "movs_ sse->sse back to movsd"
           '(#xf2 #x0f #x10 #xc9)
           (u8vector->list b))))

;; ((movs_ :op) (%rax) %xmm1)  -- mem->sse form
;; Default encoding: F2 0F 10 ModRM
;; ModRM: mod=0, reg=1(xmm1/dst), r/m=0(%rax) -> 0b00_001_000 = #x08
(let ()
  (define tmpl (asm-template '(((movs_ :op) (%rax) %xmm1))))
  (receive (b _) (link-template tmpl '())
    (test* "movs_ mem->sse default (movsd)"
           '(#xf2 #x0f #x10 #x08)
           (u8vector->list b)))
  (receive (b _) (link-template tmpl '((:op #f movss)))
    (test* "movs_ mem->sse switched to movss"
           '(#xf3 #x0f #x10 #x08)
           (u8vector->list b))))

;; ((movs_ :op) %xmm0 (%rax))  -- sse->mem form
;; Default encoding: F2 0F 11 ModRM
;; ModRM: mod=0, reg=0(xmm0/src), r/m=0(%rax) -> #x00
(let ()
  (define tmpl (asm-template '(((movs_ :op) %xmm0 (%rax)))))
  (receive (b _) (link-template tmpl '())
    (test* "movs_ sse->mem default (movsd)"
           '(#xf2 #x0f #x11 #x00)
           (u8vector->list b)))
  (receive (b _) (link-template tmpl '((:op #f movss)))
    (test* "movs_ sse->mem switched to movss"
           '(#xf3 #x0f #x11 #x00)
           (u8vector->list b))))

;; movs_ combined with other placeholders in a sequence
(let ()
  (define tmpl (asm-template '((.dataq :fn-ptr)
                                ((movs_ :variant) (%rax) %xmm0)
                                (ret))))
  (receive (b _) (link-template tmpl `((:fn-ptr ,<uint64> #xdeadbeef) (:variant #f movss)))
    (test* "movs_ combined with .dataq placeholder"
           (append '(#xef #xbe #xad #xde 0 0 0 0) ; .dataq :fn-ptr
                   '(#xf3 #x0f #x10 #x00)          ; movss (%rax),%xmm0
                   '(#xc3))                         ; ret
           (u8vector->list b))))

(test-end)
