;;; liskk.el --- Yet another ddskk (Daredevil Simple Kana to Kanji conversion)      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: lisp settings
;; Version: 0.1.0
;; URL: https://github.com/conao3/liskk.el
;; Package-Requires: ((emacs "24.4"))

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
(defvar liskk-rule-tree nil)

(defcustom liskk-mode-base-lighter " liskk"
  "Base lighter for `liskk-mode'."
  :type 'string
  :group 'liskk)

(defcustom liskk-mode-kana-lighter "[かな]"
  "Base lighter for `liskk-mode'."
  :type 'string
  :group 'liskk)

(defcustom liskk-mode-katakana-lighter "[カナ]"
  "Base lighter for `liskk-mode'."
  :type 'string
  :group 'liskk)

(defcustom liskk-mode-ascii-lighter "[半英]"
  "Base lighter for `liskk-mode'."
  :type 'string
  :group 'liskk)

(defcustom liskk-mode-zen-ascii-lighter "[全英]"
  "Base lighter for `liskk-mode'."
  :type 'string
  :group 'liskk)

(defcustom liskk-mode-abbrev-lighter "[aあ]"
  "Base lighter for `liskk-mode'."
  :type 'string
  :group 'liskk)

(defcustom liskk-preface-dict-buffer-name " *liskk-preface-dict-%s*"
  "Buffer name for `liskk-preface-dict-path-list'."
  :type 'string
  :group 'liskk)

(defcustom liskk-personal-dict-buffer-name " *liskk-personal-dict*"
  "Buffer name for `liskk-personal-dict-path'."
  :type 'string
  :group 'liskk)

(defcustom liskk-shared-dict-buffer-name " *liskk-shared-dict-%s*"
  "Buffer name for `liskk-shared-dict-path-list'."
  :type 'string
  :group 'liskk)

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
                      (list (list nkey nil nnext nil nil)))
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
  (setq liskk-rule-tree nil)
  (mapc
   (lambda (elm)
     (liskk-compile-rule-tree-add liskk-rule-tree (car elm) elm))
   (mapcan 'reverse (list (liskk-alist-get method liskk-rule-roman-kana-base-alist)
                          (liskk-alist-get method liskk-rule-roman-kana-alist)))))

(defun liskk-prepare-dict ()
  "Prepare dictionary."
  (dolist (elm '(liskk-preface-dict-path-list liskk-shared-dict-path-list))
    (mapc
     (lambda (el)
       (let ((filename (file-name-nondirectory el)))
         (when (and (not (file-readable-p el))
                    (member filename liskk-well-known-dictionary-name))
           (with-temp-file el
             (url-insert-file-contents
              (format liskk-dict-download-url filename))))))
     elm))

  (when liskk-preface-dict-path-list
    (dolist (num (number-sequence 1 (length liskk-preface-dict-path-list)))
      (with-current-buffer (get-buffer-create
                            (format liskk-preface-dict-buffer-name num))
        (erase-buffer)
        (insert-file-contents (nth (1- num) liskk-preface-dict-path-list)))))

  (when liskk-personal-dict-path
    (with-current-buffer (get-buffer-create liskk-personal-dict-buffer-name)
      (erase-buffer)
      (insert-file-contents liskk-personal-dict-path)))

  (when liskk-shared-dict-path-list
    (dolist (num (number-sequence 1 (length liskk-shared-dict-path-list)))
      (with-current-buffer (get-buffer-create
                            (format liskk-shared-dict-buffer-name num))
        (erase-buffer)
        (insert-file-contents (nth (1- num) liskk-shared-dict-path-list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minor-mode
;;

;; katakana ascii zen-ascii abbrev
(defvar-local liskk-internal-mode 'kana)

(define-minor-mode liskk-mode
  "Yet another ddskk (Daredevil Simple Kana to Kanji conversion)."
  :require 'liskk
  :lighter liskk-mode-base-lighter
  :group 'liskk
  (unless liskk-initialize-p
    (liskk-prepare-dict)
    (liskk-compile-rule-tree-make 'roman)
    (setq liskk-initialize-p t))

  (if liskk-mode
      (progn
        (setq-local liskk-internal-mode 'kana))))

(provide 'liskk)
;;; liskk.el ends here
