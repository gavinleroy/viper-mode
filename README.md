# Viper Mode

Viper Mode is an Emacs major mode for editing [Viper](https://www.pm.inf.ethz.ch/research/viper.html) files. 

:warning: please ignore the `lsp-*` files as the language server was never completed (nor is it worth my time to complete them right now). 
For interaction please download the suite of [command line tools](https://www.pm.inf.ethz.ch/research/viper/downloads.html) and add something 
similar as the following (*hack*) to your `config.el`:

```elisp
(add-to-list 'load-path <path to viper-mode>)

(require 'viper-mode)

(eval-after-load 'viper-mode
  (progn (setenv "Z3_EXE" "<viper tools>/z3/bin/z3")
         (setenv "BOOGIE_EXE" "<viper tools>/boogie/Binaries/Boogie")
         (viper-enable-format-on-save)))

(defun verify-buffer ()
  "Verify the current buffer with Carbon backend."
  (interactive)
  (let ((java (executable-find "java"))
        (carbon-jar "<viper tools>/carbon/target/scala-2.13/carbon.jar")
        (fn (buffer-file-name (current-buffer))))
    (async-shell-command
     (concat (shell-quote-argument java)
             " -jar " carbon-jar " " fn))))

(add-hook 'viper-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-v") 'verify-buffer)))
```

Verification of the open file can be run with `"C-c C-v"` and this mode will include and approximation of the Viper-IDE syntax highlighting and 
formatting.

## Disclaimer

This repository is not affiliated with the Programming Methodology Group at ETHZ and is a personal project to allow me to avoid VSCode
while doing coursework that requires me to use Viper. 

Enjoy,

:beers:
