;;; server-menu.el --- server-connection-selector    -*- lexical-binding: t; -*-
;;
;; Author: wild <wild5r@github>
;; Keywords: tools
;;
;;; Commentary:
;;
;;  Server Menu is an Emacs package that provides a simple Emacs plugin for
;;  managing and connecting to SSH servers with custom configurations using
;;  an external terminal emulator.
;;
;;; File format:
;; user1@server1.com title:production option:LC_ALL=en_US.UTF-8
;; user2@server2.com title:staging option:TERM=xterm-256color
;; user1@server3.com title:database rc:"LC_ALL=en_US.UTF-8 screen -D -RR -h 20000"
;;;
;;
;;; Code:

(require 'vertico)

(defvar ssh-servers-file (expand-file-name "~/.config/server-menu.txt")
  "Path to the file containing the list of servers.")

(defvar server-menu-terminal (lambda () "gnome-terminal")
  "Function or string returning the terminal emulator to use.
Defaults to gnome-terminal. If it is a function, it will be called.
Expected to return a command name that is executable.")

(defun ssh-servers-read-file ()
  "Read the list of servers from the file.
Each line format: \\='username@hostname title:host1 option:KEY=VALUE rc:\"command\"\\='"
  (with-temp-buffer
    (insert-file-contents ssh-servers-file)
    (let (servers)
      (while (not (eobp))
        (let* ((line (string-trim (buffer-substring-no-properties
                                   (point) (progn (forward-line 1) (point)))))
               (parts (split-string line " "))
               (connection (car parts))
               (title (when (string-match "title:\\([^[:space:]]+\\)" line)
                        (match-string 1 line)))
               (options (when (string-match "option:\\([^[:space:]]+\\)" line)
                          (match-string 1 line)))
               (rc (when (string-match "rc:\"\\([^\"]+\\)\"" line)
                     (match-string 1 line))))
          (unless (string-empty-p line)
            (push (list connection title options rc) servers))))
      (nreverse servers))))

(defun server-menu()
  "Interactive function to connect to a server using vertico and `completing-read`."
  (interactive)
  (let* ((servers (ssh-servers-read-file))
         (server-titles (mapcar (lambda (server)
                                  (or (nth 1 server) (car server)))
                                servers))
         (selected-title (completing-read "Select server: " server-titles))
         (selected-server (seq-find (lambda (server)
                                      (or (string= (nth 1 server) selected-title)
                                          (string= (car server) selected-title)))
                                    servers))
         (debug-buffer (get-buffer-create "*server-menu*")))
    (when selected-server
(let* ((connection (car selected-server))
       (options (nth 2 selected-server))
       (rc (nth 3 selected-server))
       (terminal (if (functionp server-menu-terminal)
                     (funcall server-menu-terminal)
                   server-menu-terminal))
       (ssh-command (cond
                    ;; Both options and rc
                    ((and options rc)
                     (format "env %s ssh %s -t '%s'" options connection rc))
                    ;; Only rc
                    (rc
                     (format "ssh %s -t '%s'" connection rc))
                    ;; Only options
                    (options
                     (format "env %s ssh %s" options connection))
                    ;; Neither
                    (t
                     (format "ssh %s" connection)))))
  (with-current-buffer debug-buffer
    (goto-char (point-max))
    (insert (format "Selected server: %s\n" selected-server))
    (insert (format "Executing command: %s\n" ssh-command))
    (cond
     ((and terminal (executable-find terminal))
      (cond
       ((string= terminal "gnome-terminal")
        (start-process "ssh" debug-buffer "gnome-terminal" "-p" "-v" "--" "bash" "-c" ssh-command))
       ((string= terminal "konsole")
        (start-process "ssh" nil "konsole" "-e" "bash" "-c" ssh-command))
       ((string= terminal "xterm")
        (start-process "ssh" nil "xterm" "-e" "bash" "-c" ssh-command))
        (t (start-process "ssh" nil terminal "-e" "bash" "-c" (format "TERM=xterm-256color %s" ssh-command)))))

     ((executable-find "gnome-terminal")
      (start-process "ssh" debug-buffer "gnome-terminal" "-p" "-v" "--" "bash" "-c" ssh-command))
     ((executable-find "konsole")
      (start-process "ssh" nil "konsole" "-e" "bash" "-c" ssh-command))
     ((executable-find "xterm")
      (start-process "ssh" nil "xterm" "-e" "bash" "-c" ssh-command))
     (t (error "No suitable terminal emulator found"))))))))


(provide 'server-menu)
;;; server-menu.el ends here
