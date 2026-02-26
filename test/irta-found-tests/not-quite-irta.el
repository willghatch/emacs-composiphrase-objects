;;; -*- lexical-binding: t; -*-

(require 'carettest-tesmo)
(require 'carettest-tesmut)
(require 'cpo-text-object-stuff)

(carettest-tesmut-test
 test-smartparens-modifications-cpo-smartparens-open-sibling-backward__ucdsvt___not-really-an-irta-test-it-passes
 :before "(foo bar<p>)"
 :after "(foo <p> bar)"
 :function 'cpo-smartparens-open-sibling-backward
 :transient-mark-mode t
 :setup (progn
          (smartparens-mode 1)
          (sp-local-pair 'emacs-lisp-mode "\"" "\"")))

(carettest-tesmut-test
 test-smartparens-modifications-wrap-with-brackets__mitnhu
 :before "(a [b c]<p> d)"
 :after "(a [[b c]]<p> d)"
 :function (lambda nil
             (cpo-smartparens-wrap-with-delimiter "["))
 :transient-mark-mode t
 :setup (progn
          (smartparens-mode 1)
          (sp-local-pair 'emacs-lisp-mode "\"" "\"")))
