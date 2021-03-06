;;; liskk.el --- Yet another ddskk (Daredevil Simple Kana to Kanji conversion)      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: lisp settings
;; Version: 0.1.0
;; URL: https://github.com/conao3/liskk.el
;; Package-Requires: ((emacs "24.4") (ov "1.0.6"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the Affero GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the Affero
;; GNU General Public License for more details.

;; You should have received a copy of the Affero GNU General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; liskk is yat another ddskk

;;; Code:

(require 'liskk-polyfill)
(require 'liskk-rule)

(defgroup liskk nil
  "Yet another ddskk (Daredevil Simple Kana to Kanji conversion)."
  :group 'lisp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Customization
;;

(defvar liskk-initialize-p nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minor-mode Lighter
;;

(defcustom liskk-mode-base-lighter " liskk"
  "Base lighter for `liskk-mode'."
  :type 'string
  :group 'liskk)

(defcustom liskk-kana-mode-lighter '("[かな]" "[カナ]" "[半カナ]")
  "The lighter for internal `liskk-kana-mode' for `liskk-mode'."
  :type 'string
  :group 'liskk)

(defcustom liskk-ascii-mode-lighter '("[半英]" "[全英]")
  "The lighter for internal `liskk-ascii-mode' for `liskk-mode'."
  :type 'string
  :group 'liskk)

(defcustom liskk-abbrev-mode-lighter '("[aあ]")
  "The lighter for internal `liskk-abbrev-mode' for `liskk-mode'."
  :type 'string
  :group 'liskk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Buffer name
;;

(defcustom liskk-dict-buffer-name " *liskk-dict-%s*"
  "Buffer name for `liskk-mode' dictionary buffer."
  :type 'string
  :group 'liskk)

(defcustom liskk-debug-buffer-name "*liskk-debug*"
  "Buffer name for `liskk-mode' debug buffer."
  :type 'string
  :group 'liskk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Dictionary
;;

(defvar liskk-well-known-dictionary-name
  '("SKK-JISYO.L" "SKK-JISYO.ML" "SKK-JISYO.M" "SKK-JISYO.S"
    "SKK-JISYO.JIS2" "SKK-JISYO.JIS3_4" "SKK-JISYO.pubdic+"
    "SKK-JISYO.wrong.annotated" "SKK-JISYO.okinawa" "SKK-JISYO.geo"
    "SKK-JISYO.jinmei" "SKK-JISYO.law" "SKK-JISYO.mazegaki"
    "SKK-JISYO.assoc" "SKK-JISYO.itaiji" "SKK-JISYO.itaiji.JIS3_4"
    "SKK-JISYO.china_taiwan" "SKK-JISYO.propernoun" "SKK-JISYO.station"
    "SKK-JISYO.requested" "SKK-JISYO.fullname" "SKK-JISYO.JIS2004"
    "SKK-JISYO.lisp"))

(defcustom liskk-dict-download-url
  "https://raw.githubusercontent.com/conao3/liskk-dict.el/master/utf-8/%s"
  "The url to download dictionary."
  :type 'string
  :group 'liskk)

(defcustom liskk-preface-dict-path-list
  (list (locate-user-emacs-file "liskk/preface.L"))
  "Dictionary list to search before searching the personal dictionary.
個人辞書の検索の前に検索する辞書。
見出し語は、ソートされていなければならない。
Non-nilであれば、指定された辞書を検索のためバッファに読み込み、検索を行う。

`liskk-well-known-dictionary-name'に含まれるファイル名を指定した場合、
ファイルが存在しなければ自動的にダウンロードする。"
  :type 'sexp
  :group 'liskk)

(defcustom liskk-personal-dict-path
  (locate-user-emacs-file "liskk/personal.L")
  "Personal dictionary file path and save path.
個人辞書のパス。また、このパスに個人辞書を保存する。
見出し語は、ソートされていなければならない。 nilの場合、個人辞書の保存を行わない。"
  :type 'file
  :group 'liskk)

(defcustom liskk-shared-dict-path-list
  (list (locate-user-emacs-file "liskk/SKK-JISYO.L"))
  "Dictionary list to search after searching the personal dictionary.
個人辞書の検索の後に検索する辞書。
見出し語は、ソートされていなければならない。
Non-nilであれば、指定された辞書を検索のためバッファに読み込み、検索を行う。

`liskk-well-known-dictionary-name'に含まれるファイル名を指定した場合、
ファイルが存在しなければ自動的にダウンロードする。"
  :type 'sexp
  :group 'liskk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Roman-kana conversion rule
;;

(defconst liskk-rule-roman-kana-base-alist
  `((roman . ,liskk-rule-roman-kana-base))
  "The conversion rule roman to kana.

alistは次の形式である:
  <alist>     := nil | (<rule-cell>*)
  <rule-cell> := (<method> . (<rule>*))
  <rule>      := (<input> <next> <output>)
  <method>    := [symbol]; 入力方法の識別のためのシンボル
  <input>     := [string]; ルール実行のために必要な入力
  <next>      := [string]; ルール実行後に遷移する状態
  <output>    := [string]; ルール実行後に挿入される文字(列)(ひらがなで指定する)
                   | [symbol]; ルール実行後に関数を実行する

<rule>の例:
  (\"a\" nil (\"あ ア\"))
  (\"ki\" nil (\"き キ\"))
  (\"tt\" t (\"っ ッ\"))
  (\"q\" nil liskk-toggle-kana)

  a  => あ
  ki => き
  tt => っt
  q  => liskk-toggle-kana")

(defcustom liskk-rule-roman-kana-alist nil
  "Addtional conversionrule roman to kana.

*状態遷移規則のリストで、ユーザの追加設定用の変数。

この変数は、`liskk-rom-kana-base-rule-alist' と同様の書式を満たす必要がある。

LISKK は起動時にこの 2 変数を編集して `liskk-rule-tree' を作成するが、
`liskk-rule-roman-kana-alist' の規則は `liskk-rule-roman-kana-base-alist' の規則よりも
優先される。

ユーザは、追加したい規則を、例えば

    (setq liskk-rule-roman-kana-alist
      '((\"hh\" \"h\" (\"ッ\" . \"っ\"))
        (\"@\" nil \"＠\")
        ...
        ))

上記のように設定することができる。

`@' で `liskk-today' (当日の日付の入力) を起動する代りに `＠' を入
力したい場合は、`liskk-rule-roman-kana-alist' に

    (\"@\" nil \"＠\")

という要素を加える。

もし、LISKK を起動した後で `liskk-rule-roman-kana' を変更した場合、その設
定を反映させるには \\[liskk-restart] を実行する必要がある。"
  :type '(repeat
          (cons (symbol :tag "入力方法")
                (list :tag "ルール"
                      (radio :tag "1 入力"
                             (string :tag "文字列")
                             (symbol :tag "変数名"))
                      (radio :tag "2 次の状態"
                             (string :tag "文字列")
                             (const :tag "nil (空の状態)" nil))
                      (radio :tag "3 出力"
                             (string :tag "文字列")
                             (function :tag "関数を実行する")))))
  :group 'liskk)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Implemention
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Declare symbols
;;

(defvar liskk-mode)
(defvar liskk-debug-mode)
(defvar liskk-kana-mode)
(defvar liskk-ascii-mode)
(defvar liskk-abbrev-mode)
(defvar liskk-internal-modes '(kana ascii abbrev debug))
(defvar-local liskk-internal-type 0)
(defvar-local liskk-current-rule-node nil)
(defvar-local liskk-ov-roman-fragment nil)

;; below variables are reused all `liskk-mode' process
(defvar liskk-rule-tree nil)

(defvar liskk-mode-map (make-sparse-keymap)
  "Keymap for `liskk-mode'.")

(defvar liskk-kana-mode-map
  (let ((keymap (make-sparse-keymap)))
    (dotimes (i 95)
      (define-key keymap (char-to-string (+ 32 i)) #'liskk-self-insert))
    keymap)
  "Keymap for `liskk-kana-mode'.")

(defvar liskk-ascii-mode-map
  (let ((keymap (make-sparse-keymap)))
    (dotimes (i 95)
      (define-key keymap (char-to-string (+ 32 i)) #'liskk-self-insert))
    keymap)
  "Keymap for `liskk-ascii-mode'.")

(defvar liskk-abbrev-mode-map
  (let ((keymap (make-sparse-keymap)))
    (dotimes (i 95)
      (define-key keymap (char-to-string (+ 32 i)) #'liskk-self-insert))
    keymap)
  "Keymap for `liskk-abbrev-mode'.")

(defvar liskk-debug-mode-map
  (let ((keymap (make-sparse-keymap)))
    (dotimes (i 95)
      (define-key keymap (char-to-string (+ 32 i)) #'liskk-self-insert))
    keymap)
  "Keymap for `liskk-debug-mode'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  liskk main routine
;;

(defun liskk-self-insert (arg)
  "LISKK version of `self-insert-command'."
  (interactive "p")
  (let ((key last-command-event))
    (when (< 0 arg)
      (dotimes (i arg)
        (when liskk-debug-mode
          (with-current-buffer (get-buffer-create liskk-debug-buffer-name)
            (goto-char (point-max))
            (insert (format "self-insert(%d): %c\n" i key))))
        (cond
         (liskk-kana-mode
          (liskk-kana-input key))
         (liskk-ascii-mode)
         (liskk-abbrev-mode))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  liskk kana mode
;;

(defun liskk-kana-insert (node)
  "Insert kana."
  (when liskk-debug-mode
    (with-current-buffer (get-buffer-create liskk-debug-buffer-name)
      (goto-char (point-max))
      (insert (format "kana-insert: %s\n"
                      (truncate-string-to-width (prin1-to-string node) 60)))))
  (when (nth 3 node)
    (insert (nth 3 node)))
  (when (nth 2 node)
    (dolist (key (split-string (nth 2 node) "" 'omit))
      (liskk-kana-input (string-to-char key)))))

(defun liskk-kana-input (key)
  "Input key and convert kana.

かな文字の入力を行うルーチン。

Message-Id: <19980610190611B.sakurada@kuis.kyoto-u.ac.jp>
From: Hideki Sakurada <sakurada@kuis.kyoto-u.ac.jp>
Date: Wed, 10 Jun 1998 19:06:11 +0900 (JST)

新しい skk-kana-input は, 簡単に説明すると,あらかじめルールを木の形
に表現しておいて, 入力を見て木を辿り, それ以上辿れなくなったらその節
に登録されている仮名を入力する. というしくみです.

例えば, n a t のみからなる以下のルール

    a  → あ
    n  → ん
    nn → ん
    na → な
    ta → た
    tt → っ (次状態 t)

をルール木に変換すると,

                ／/＼
              ／ /   ＼
          a ／  / t    ＼ n
          ／   /         ＼
         あ   ・           ん
            ／|           / ＼
        a ／  | t      a /    ＼ n
        ／    |         /       ＼
      た     っ        な        ん
         (次状態 \"t\")

という形になります.

内部形式では次のように表現されます。簡便のために引用符を省略します

  (let ((liskk-rule-tree '(nil nil nil nil nil)))
        (mapc
         (lambda (elm)
           (liskk-compile-rule-tree-add liskk-rule-tree (car elm) elm))
         (reverse '((a nil あ)
                    (n nil ん)
                    (nn nil ん)
                    (na nil な)
                    (ta nil た)
                    (tt t っ))))
        liskk-rule-tree)
;; => (nil nil nil nil
;;         ((97 a nil あ nil)
;;          (110 n nil ん
;;               ((110 nn nil ん nil)
;;                (97 na nil な nil)))
;;          (116 t nil nil
;;               ((97 ta nil た nil)
;;                (116 tt t っ nil)))))

初期状態(木の根)で `a' を入力すると, 木の根から「あ」に移動します.
次にどのような入力が来ても,それより下に辿れないので, 「あ」を出力し
て根に戻ります. ルールに次状態が設定されている場合は, 設定されている
文字列をキューに戻してから根に戻ります.

初期状態で `n' を入力すると, 「ん」に移動します. 次に `a' または `n'
が入力されればそれより下に辿れるので次の入力を見るまでまだ出力しませ
ん. 次に `t' が入力された場合は, `t' では下に辿れないので,「ん」を出
力して `t' をキューに戻します."
  (when liskk-debug-mode
    (with-current-buffer (get-buffer-create liskk-debug-buffer-name)
      (goto-char (point-max))
      (insert (format "kana-input: %c\n" key))
      (insert (format "current-state: %s\n"
                      (truncate-string-to-width
                       (prin1-to-string liskk-current-rule-node) 60)))))

  ;; 状態がない場合
  ;;   - 現在の状態を根として処理を始める
  ;;   - ローマ字断片オーバーレイを移動させる
  (unless liskk-current-rule-node
    (setq-local liskk-current-rule-node liskk-rule-tree)
    (if liskk-ov-roman-fragment
        (liskk-ov-move-here liskk-ov-roman-fragment (point))
      (setq-local liskk-ov-roman-fragment (liskk-ov-here (point)))))

  ;; 現在の葉から次の状態に遷移しようとする
  (if (assoc key (nth 4 liskk-current-rule-node))
      (progn
        ;; 次の状態に遷移できた
        ;;   - 状態を更新する
        ;;   - ローマ字断片オーバーレイの表示を更新する
        ;;   - 次状態遷移可能性を検証する
        (setq-local liskk-current-rule-node (assoc key (nth 4 liskk-current-rule-node)))
        (liskk-ov-aset (it liskk-ov-roman-fragment) 'after-string
          (propertize
           (format "%s%c" (substring-no-properties (or it "")) key)
           'face 'font-lock-warning-face))

        (unless (nth 4 liskk-current-rule-node)
          ;; 遷移できたが、次の状態がない
          ;;   - 根に戻る
          ;;   - ローマ字断片オーバーレイを消す
          ;;   - 最終の状態で`liskk-kana-insert'を実行する
          ;;     - 現在の葉の文字列を挿入する
          ;;     - 葉に次状態の指定があれば、それを処理器に投入する
          (let ((node liskk-current-rule-node))
            (setq-local liskk-current-rule-node nil)
            (liskk-ov-discard liskk-ov-roman-fragment 'after-string)
            (liskk-kana-insert node))))

    (let ((node liskk-current-rule-node))
      ;; 次の状態に遷移できない
      ;;   - 根に戻る
      ;;   - ローマ字断片オーバーレイを消す
      ;;   - 最終の状態で`liskk-kana-insert'を実行する
      ;;     - 現在の葉の文字列を挿入する
      ;;     - 葉に次状態の指定があれば、それを処理器に投入する
      ;;   - 入力されたキーを、遷移の可能性がある場合、次のローマ字列として処理器に投入する
      (setq-local liskk-current-rule-node nil)
      (liskk-ov-discard liskk-ov-roman-fragment 'after-string)
      (liskk-kana-insert node)
      (when (assoc key (nth 4 liskk-rule-tree))
        (liskk-kana-input key))))

  (when liskk-debug-mode
    (with-current-buffer (get-buffer-create liskk-debug-buffer-name)
      (goto-char (point-max))
      (insert (format "moved-state: %s\n\n"
                      (truncate-string-to-width
                       (prin1-to-string liskk-current-rule-node) 60))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Roman-kana rule
;;

(defun liskk-compile-rule-tree-add (current-node str node)
  "Add NODE and STR to CURRENT-NODE."
  (let* ((nkey     (aref str 0))
         (nkeyrest (substring str 1))
         (nstr     (nth 0 node))
         (nnext    (nth 1 node))
         (nout     (nth 2 node))
         (statep   (= 1 (length str))))
    (if (assoc nkey (nth 4 current-node))
        (progn
          (if statep
              (progn
                (setf (nth 0 (assoc nkey (nth 4 current-node))) nkey)
                (setf (nth 1 (assoc nkey (nth 4 current-node))) nstr)
                (setf (nth 2 (assoc nkey (nth 4 current-node))) nnext)
                (setf (nth 3 (assoc nkey (nth 4 current-node))) nout))
            (liskk-compile-rule-tree-add (assoc nkey (nth 4 current-node)) nkeyrest node)))
      (setf (nth 4 current-node)
            (append (if statep
                        (list (list nkey nstr nnext nout nil))
                      (list (list nkey (char-to-string nkey) nil nil nil)))
                    (nth 4 current-node)))
      (unless statep
        (liskk-compile-rule-tree-add (assoc nkey (nth 4 current-node)) nkeyrest node)))))

(defun liskk-compile-rule-tree-make (method)
  "Compile RULE-LISTS to `liskk-rule-tree'.
RULE-LISTSを木の形にコンパイルする。

Treeは次の形式である:
  <tree> := nil | (<key> <str> <next> <out> (<tree>*))
  <key>  := nil | [char];   該当の木に遷移するキー
  <str>  := nil | [string]; 該当の葉に遷移するために必要な全体のキー
  <next> := nil | [string]; 該当の葉に遷移した後に遷移する状態
  <out>  := nil | [string]; 該当の葉に遷移した時に挿入される文字(列)

なお、<key>がnilの場合、その葉は根であり、
<str>や<out>がnilの場合、その葉によって挿入される文字列はないことを示す。"
  (setq liskk-rule-tree '(nil nil nil nil nil))
  (mapc
   (lambda (elm)
     (liskk-compile-rule-tree-add liskk-rule-tree (car elm) elm))
   (mapcan 'reverse (list (liskk-alist-get method liskk-rule-roman-kana-base-alist)
                          (liskk-alist-get method liskk-rule-roman-kana-alist))))
  liskk-rule-tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Dictionary
;;

(defun liskk-prepare-dict ()
  "Prepare dictionary."
  (dolist (elm '(liskk-preface-dict-path-list liskk-shared-dict-path-list))
    (mapc
     (lambda (el)
       (let ((filename (file-name-nondirectory el)))
         (when (and (not (file-readable-p el))
                    (member filename liskk-well-known-dictionary-name))
           (or (file-directory-p (file-name-directory el))
               (mkdir (file-name-directory el) 'parent))
           (with-temp-file el
             (url-insert-file-contents (format liskk-dict-download-url filename))))))
     (symbol-value elm)))

  (let ((num 0))
    (dolist (elm (append liskk-preface-dict-path-list
                         (list liskk-personal-dict-path) ; not list
                         liskk-shared-dict-path-list))
      (setq num (1+ num))
      (when (file-readable-p elm)
        (with-current-buffer (get-buffer-create (format liskk-dict-buffer-name num))
          (erase-buffer)
          (insert-file-contents elm))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minor-mode
;;

(define-minor-mode liskk-mode
  "Yet another ddskk (Daredevil Simple Kana to Kanji conversion)."
  :require 'liskk
  :lighter liskk-mode-base-lighter
  :group 'liskk
  :keymap liskk-mode-map
  (unless liskk-initialize-p
    (liskk-prepare-dict)
    (liskk-compile-rule-tree-make 'roman)
    (setq liskk-initialize-p t))

  (if liskk-mode
      (progn
        (eval `(,(intern (format "liskk-kana-mode")) +1))
        (setq-local liskk-internal-type 0))
    (eval
     `(progn
        ,@(mapcar
           (lambda (elm)
             `(,(intern (format "liskk-%s-mode" (symbol-name elm))) -1))
           liskk-internal-modes)))))

(eval
 `(progn
    ,@(mapcan
       (lambda (elm)
         (let ((sym (intern (format "liskk-%s-mode" (symbol-name elm)))))
           `((defvar ,sym nil
               ,(format "Non-nil if %s is enabled.
Use the command to change this variable." (symbol-name sym)))
             (defun ,sym (&optional arg)
               ,(format "Internal mode for `liskk-mode' to insert %s." (symbol-name elm))
               (setq ,sym
                     (if (eq arg 'toggle)
                         (not liskk-kana-mode)
                       (< 0 (prefix-numeric-value arg))))
               (if ,sym
                   (progn
                     (unless liskk-mode
                       (liskk-mode 1) (,sym +1))
                     ,@(mapcar
                        (lambda (el)
                          `(,(intern (format "liskk-%s-mode" (symbol-name el))) -1))
                        (remove elm liskk-internal-modes)))
                 (setq-local liskk-current-rule-node nil)
                 (liskk-ov-discard liskk-ov-roman-fragment 'after-string))
               (run-hooks 'liskk-kana-mode-hook
                          (if liskk-kana-mode 'liskk-kana-mode-on-hook 'liskk-kana-mode-off-hook))
               liskk-kana-mode)

             (defvar ,(intern (format "liskk-%s-mode-hook" (symbol-name elm))) nil
               ,(format "Hook run after entering or leaving `%s'.
No problems result if this variable is not bound.
`add-hook' automatically binds it.  (This is true for all hook variables.)"
                        (symbol-name sym)))
             (with-no-warnings
               (add-minor-mode
                ',sym 'nil ,(intern (format "liskk-%s-mode-map" (symbol-name elm))))))))
       liskk-internal-modes)))

(provide 'liskk)
;;; liskk.el ends here
