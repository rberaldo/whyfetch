;;; whyfetch.el --- Yahoo Finance stock fetcher for ledger -*- lexical-binding: t; -*-

;;; Copyleft (Ⓚ) 2022   Rafael Beraldo
;;; URL: https://github.com/rberaldo/whyfetch
;;; Version: 0.1
;;; Package-Requires: ((plz "0.2") (emacs "25.1"))

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Fetches stock prices from Yahoo Finance and updates entries in a
;; ledger commmodity price database.

;; Usage: In a ledger commodity price database file, run M-x
;; whyfetch-update-ledger-db-prices. whyfetch will list all
;; commodities and add a new price for each.

;; If you have a Bitcoin entry with the commodity code BTC, then
;; whyfetch will not only get the latest price in USD, but also
;; convert it to the currency at the end of the entry. For example, if
;; whyfetch finds the line

;; P 2022/10/04 18:39:45 "BTC" 105205.49 BRL

;; it will know to convert BTC into BRL. Other coins are not
;; supported for the time being.

;;; Code:

(require 'plz)

(defun whyfetch--list-commodities ()
  "List commodities in a ledger database file."
  (goto-char (point-min))
  (let (line commodity commodity-list)
    (while (re-search-forward "^P" nil t)
      (setq line (buffer-substring-no-properties
		  (line-beginning-position) (line-end-position)))
      (setq commodity (whyfetch--get-commodity-or-currency line 'commodity))
      (add-to-list 'commodity-list commodity))
    commodity-list))

(defun whyfetch--copy-down-commodity-price ()
  "Add an updated commodity price entry based on the current line.
It should follow the structure \"P DATE TIME COMMODITY PRICE
CURRENCY\". Automatically updates date, time and fetches current
prices."
  (let (line commodity currency price)
    (setq line (buffer-substring-no-properties
		(line-beginning-position) (line-end-position)))
    (setq commodity (whyfetch--get-commodity-or-currency line 'commodity))
    (setq currency (whyfetch--get-commodity-or-currency line 'currency))
    ;; TODO: Add NIL check at this point.
    (setq price (format "%0.2f"
			(if (equal commodity "BTC")
			    (whyfetch--fetch-yahoo-finance-BTC-price currency)
			  (whyfetch--fetch-yahoo-finance-commodity-price commodity))))
    (move-end-of-line 1)
    (newline)
    (whyfetch--insert-commodity-price commodity price currency)))

(defun whyfetch--get-commodity-or-currency (entry prop)
  "Get PROP, a commodity name or the currency, in a ledger price ENTRY.
ENTRY has to be formatted according to the ledger price database
format."
  (let (line)
    (setq line (split-string entry))
    (pcase prop
      ('commodity (string-trim (nth 3 line) "\"" "\""))
      ('currency (string-trim (nth 5 line) "\"" "\"")))))

(defun whyfetch--insert-commodity-price (c p m)
  "Insert a price entry with commodity C, price P and currency M."
  (insert (concat
	   "P "
	   (format-time-string "%Y/%m/%d %T ")
	   "\"" c "\""
	   " " p
	   " " m)))

(defun whyfetch--fetch-yahoo-finance-commodity-price (c)
  "Fetch commodity C prices from Yahoo Finance.
Return nil if commodity is not found."
  (let (yahoo-base-url price-json)
    (setq yahoo-base-url "https://query1.finance.yahoo.com/v8/finance/chart/")
    (setq price-json
	  (ignore-errors (plz 'get (concat yahoo-base-url c) :as #'json-read)))
    (if price-json
	(map-nested-elt (aref
			 (map-nested-elt price-json '(chart result))
			 0)
			'(meta regularMarketPrice))
      nil)))

(defun whyfetch--fetch-yahoo-finance-BTC-price (m)
  "Fetch BTC price from Yahoo Finances, converting from USD to M.
If currency M is not equal to USD, convert BTC ratings to the
currency."
  (let (btc-price usd-to-m)
    (setq btc-price (whyfetch--fetch-yahoo-finance-commodity-price "BTC-USD"))
    (if (not (equal m "USD"))
	(progn
	  (setq usd-to-m
		(whyfetch--fetch-yahoo-finance-commodity-price (concat
								m
								"=X")))
	  (* btc-price usd-to-m))
      btc-price)))

;;; User functions

;;;###autoload
(defun whyfetch-update-ledger-db-prices ()
  "Update commodity prices in a ledger price database file.
The function looks for the latest entry of all commodities in a
the database file and adds a new one below."
  (interactive)
  (dolist (c (whyfetch--list-commodities))
    (goto-char (point-max))
    (search-backward c)
    (whyfetch--copy-down-commodity-price)))

(provide 'whyfetch)
;;; whyfetch.el ends here
