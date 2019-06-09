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

(require 'liskk-rule)

(defgroup liskk nil
  "Yet another ddskk (Daredevil Simple Kana to Kanji conversion)."
  :group 'lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Customization
;;

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

(defcustom skk-preface-dict-path-list
  (list (locate-user-emacs-file "liskk/dict/preface.L"))
  "Dictionary list to search before searching the personal dictionary.
個人辞書の検索の前に検索する辞書。
見出し語は、ソートされていなければならない。
Non-nilであれば、指定された辞書を検索のためバッファに読み込み、検索を行う。"
  :type 'sexp
  :group 'liskk)

(defcustom skk-personal-dict-path
  (locate-user-emacs-file "liskk/dict/personal.L")
  "Personal dictionary file path and save path.
個人辞書のパス。また、このパスに個人辞書を保存する。
見出し語は、ソートされていなければならない。 nilの場合、個人辞書の保存を行わない。"
  :type 'file
  :group 'liskk)

(defcustom skk-public-dict-path-list
  (list (locate-user-emacs-file "liskk/dict/SKK-JISYO.L"))
  "Dictionary list to search after searching the personal dictionary.
個人辞書の検索の後に検索する辞書。
見出し語は、ソートされていなければならない。
Non-nilであれば、指定された辞書を検索のためバッファに読み込み、検索を行う。"
  :type 'sexp
  :group 'liskk)

(defconst liskk-rule-roman-kana-base-alist
  `(roman . ,liskk-rule-roman-kana-base)
  "The conversion rule roman to kana.

リストの各要素は次の形式である必要がある
  (INPUT-STATE NEXT-STATE OUTPUT)

INPUT-STATE, NEXT-STATEはstring, OUTPUTは下記の形式を指定できる
  String         - 全てのモードで指定された文字を挿入する
  List of String - モードによって挿入される文字を変更 (かなモード カナモード)
  Symbol         - 関数を実行する

INPUT-STATEを認識したら、OUTPUTを挿入/実行し、NEXT-STATEに移行する

Ex:
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

リストの各要素は、それぞれが一つの規則であり、下記の形式を満たしていなければ
ならない。

 (INPUT-STATE NEXT-STATE OUTPUT)

LISKK は INPUT-STATE を検出すると、OUTPUT をバッファに挿入し、続いて
NEXT-STATE に状態を移したうえで、入力待ち状態となる。

詳しくは、`liskk-rule-roman-kana-base-alist' の説明を参照のこと。

ユーザは、追加したい規則を、例えば

    (setq liskk-rule-roman-kana-alist
      '((\"hh\" \"h\" (\"ッ\" . \"っ\"))
	(\"@\" nil \"＠\")
	...
	))

上記のように設定することができる。

この変数は、標準では

    (\"hh\" \"h\" (\"ッ\" . \"っ\"))

の設定がされている。この規則に従うと、

    ohhonn => おっほん
    ohhira => おっひら

のように挿入される。もしこれを

    ohhonn  => おおほん
    ohhira  => おおひら

のように変更したければ、この設定

    (\"hh\" \"h\" (\"ッ\" . \"っ\"))

を削除する。

また、`@' で `liskk-today' (当日の日付の入力) を起動する代りに `＠' を入
力したい場合は、`liskk-rule-roman-kana-alist' に

    (\"@\" nil \"＠\")

という要素を加える。

もし、LISKK を起動した後で `liskk-rule-roman-kana' を変更した場合、その設
定を反映させるには \\[liskk-restart] を実行する必要がある。"
  :type '(repeat
	  (list :tag "ルール"
		(radio :tag "1 入力"
		       (string :tag "文字列")
		       (symbol :tag "変数名"))
		(radio :tag "2 次の状態"
		       (string :tag "文字列")
		       (const :tag "nil (空の状態)" nil))
		(radio :tag "3 出力"
		       (function :tag "関数を実行する")
		       (string :tag "文字列")
		       (cons :tag "文字列の組"
			     (string :tag "3-1 カタカナ")
			     (string :tag "3-2 ひらがな")))))
  :group 'liskk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Implemention
;;

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
  :group 'liskk)

(provide 'liskk)
;;; liskk.el ends here
