;; discipline.scm --- ICAO spelling discipline    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Created: 14 October 2014
;;
;; This file is part of Glean.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A discipline definition for learning the ICAO spelling alphabet used in,
;; for instance, telephony.
;;
;;; Code:

(define-module (glean disciplines spelling-alphabet discipline)
  #:use-module (glean library core-templates)
  #:export (icao-module))


;;;; Local Template

(define (letter-set id name synopsis . problems)
  (set id
       #:name name
       #:synopsis synopsis
       #:contents problems))

;;;; Exercises

(define alfa
  (letter-set 'alfa "Problems for A"
              "Problems relating to the letter A."
              (problem (q "What is the codeword for the letter ‘A’?")
                       (s "alfa"))
              (problem (q "Which of the following is correct?")
                       (s "alfa")
                       (o "apple")
                       (o "able")
                       (o "alfa"))))

(define bravo
  (letter-set 'bravo "Problems for B"
              "Problems relating to the letter B."
              (problem (q "What is the codeword for the letter ‘B’?")
                       (s "bravo"))
              (problem (q "Which of the following is correct?")
                       (s "bravo")
                       (o "bravo")
                       (o "beta")
                       (o "bee"))))

(define charlie
  (letter-set 'charlie "Problems for C"
              "Problems relating to the letter C."
              (problem (q "What is the codeword for the letter ‘C’?")
                       (s "charlie"))
              (problem (q "Which of the following is correct?")
                       (s "charlie")
                       (o "cuckoo")
                       (o "charlie")
                       (o "casablanca"))))

(define delta
  (letter-set 'delta "Problems for D"
              "Problems relating to the letter D."
              (problem (q "What is the codeword for the letter ‘D’?")
                       (s "delta"))
              (problem (q "Which of the following is correct?")
                       (s "delta")
                       (o "din")
                       (o "deft")
                       (o "delta"))))

(define echo
  (letter-set 'echo "Problems for E"
              "Problems relating to the letter E."
              (problem (q "What is the codeword for the letter ‘E’?")
                       (s "echo"))
              (problem (q "Which of the following is correct?")
                       (s "echo")
                       (o "echo")
                       (o "eternity")
                       (o "edward"))))

(define foxtrot
  (letter-set 'foxtrot "Problems for F"
              "Problems relating to the letter F."
              (problem (q "What is the codeword for the letter ‘F’?")
                       (s "foxtrot"))
              (problem (q "Which of the following is correct?")
                       (s "foxtrot")
                       (o "freddie")
                       (o "foxtrot")
                       (o "fox"))))

(define golf
  (letter-set 'golf "Problems for G"
              "Problems relating to the letter G."
              (problem (q "What is the codeword for the letter ‘G’?")
                       (s "golf"))
              (problem (q "Which of the following is correct?")
                       (s "golf")
                       (o "gun")
                       (o "gee")
                       (o "golf"))))

(define hotel
  (letter-set 'hotel "Problems for H"
              "Problems relating to the letter H."
              (problem (q "What is the codeword for the letter ‘H’?")
                       (s "hotel"))
              (problem (q "Which of the following is correct?")
                       (s "hotel")
                       (o "hotel")
                       (o "how")
                       (o "heart"))))

(define india
  (letter-set 'india "Problems for I"
              "Problems relating to the letter I."
              (problem (q "What is the codeword for the letter ‘I’?")
                       (s "india"))
              (problem (q "Which of the following is correct?")
                       (s "india")
                       (o "india")
                       (o "indigo")
                       (o "istanbul"))))

(define juliett
  (letter-set 'juliett "Problems for J"
              "Problems relating to the letter J."
              (problem (q "What is the codeword for the letter ‘J’?")
                       (s "juliett"))
              (problem (q "Which of the following is correct?")
                       (s "juliett")
                       (o "jungle")
                       (o "junta")
                       (o "juliett"))))

(define kilo
  (letter-set 'kilo "Problems for K"
              "Problems relating to the letter K."
              (problem (q "What is the codeword for the letter ‘K’?")
                       (s "kilo"))
              (problem (q "Which of the following is correct?")
                       (s "kilo")
                       (o "king")
                       (o "kangaroo")
                       (o "kilo"))))

(define lima
  (letter-set 'lima "Problems for L"
              "Problems relating to the letter L."
              (problem (q "What is the codeword for the letter ‘L’?")
                       (s "lima"))
              (problem (q "Which of the following is correct?")
                       (s "lima")
                       (o "lima")
                       (o "love")
                       (o "lurch"))))

(define mike
  (letter-set 'mike "Problems for M"
              "Problems relating to the letter M."
              (problem (q "What is the codeword for the letter ‘M’?")
                       (s "mike"))
              (problem (q "Which of the following is correct?")
                       (s "mike")
                       (o "monk")
                       (o "mike")
                       (o "mist"))))

(define november
  (letter-set 'november "Problems for N"
              "Problems relating to the letter N."
              (problem (q "What is the codeword for the letter ‘N’?")
                       (s "november"))
              (problem (q "Which of the following is correct?")
                       (s "november")
                       (o "nicaragua")
                       (o "november")
                       (o "nice"))))

(define oscar
  (letter-set 'oscar "Problems for O"
              "Problems relating to the letter O."
              (problem (q "What is the codeword for the letter ‘O’?")
                       (s "oscar"))
              (problem (q "Which of the following is correct?")
                       (s "oscar")
                       (o "oscar")
                       (o "oliver")
                       (o "ogle"))))

(define papa
  (letter-set 'papa "Problems for P"
              "Problems relating to the letter P."
              (problem (q "What is the codeword for the letter ‘P’?")
                       (s "papa"))
              (problem (q "Which of the following is correct?")
                       (s "papa")
                       (o "pin")
                       (o "punk")
                       (o "papa"))))

(define quebec
  (letter-set 'quebec "Problems for Q"
              "Problems relating to the letter Q."
              (problem (q "What is the codeword for the letter ‘Q’?")
                       (s "quebec"))
              (problem (q "Which of the following is correct?")
                       (s "quebec")
                       (o "quantum")
                       (o "queen")
                       (o "quebec"))))

(define romeo
  (letter-set 'romeo "Problems for R"
              "Problems relating to the letter R."
              (problem (q "What is the codeword for the letter ‘R’?")
                       (s "romeo"))
              (problem (q "Which of the following is correct?")
                       (s "romeo")
                       (o "ruin")
                       (o "ruler")
                       (o "romeo"))))

(define sierra
  (letter-set 'sierra "Problems for S"
              "Problems relating to the letter S."
              (problem (q "What is the codeword for the letter ‘S’?")
                       (s "sierra"))
              (problem (q "Which of the following is correct?")
                       (s "sierra")
                       (o "song")
                       (o "sierra")
                       (o "silo"))))

(define tango
  (letter-set 'tango "Problems for T"
              "Problems relating to the letter T."
              (problem (q "What is the codeword for the letter ‘T’?")
                       (s "tango"))
              (problem (q "Which of the following is correct?")
                       (s "tango")
                       (o "tina")
                       (o "tarot")
                       (o "tango"))))

(define uniform
  (letter-set 'uniform "Problems for U"
              "Problems relating to the letter U."
              (problem (q "What is the codeword for the letter ‘U’?")
                       (s "uniform"))
              (problem (q "Which of the following is correct?")
                       (s "uniform")
                       (o "us")
                       (o "uniform")
                       (o "uck"))))

(define victor
  (letter-set 'victor "Problems for V"
              "Problems relating to the letter V."
              (problem (q "What is the codeword for the letter ‘V’?")
                       (s "victor"))
              (problem (q "Which of the following is correct?")
                       (s "victor")
                       (o "victor")
                       (o "volley")
                       (o "vista"))))

(define whiskey
  (letter-set 'whiskey "Problems for W"
              "Problems relating to the letter W."
              (problem (q "What is the codeword for the letter ‘W’?")
                       (s "whiskey"))
              (problem (q "Which of the following is correct?")
                       (s "whiskey")
                       (o "whiskey")
                       (o "wanton")
                       (o "whale"))))

(define x-ray
  (letter-set 'x-ray "Problems for X"
              "Problems relating to the letter X."
              (problem (q "What is the codeword for the letter ‘X’?")
                       (s "x-ray"))
              (problem (q "Which of the following is correct?")
                       (s "x-ray")
                       (o "xanadu")
                       (o "xylophone")
                       (o "x-ray"))))

(define yankee
  (letter-set 'yankee "Problems for Y"
              "Problems relating to the letter Y."
              (problem (q "What is the codeword for the letter ‘Y’?")
                       (s "yankee"))
              (problem (q "Which of the following is correct?")
                       (s "yankee")
                       (o "yogi")
                       (o "yankee")
                       (o "yolo"))))

(define zulu
  (letter-set 'zulu "Problems for Z"
              "Problems relating to the letter Z."
              (problem (q "What is the codeword for the letter ‘Z’?")
                       (s "zulu"))
              (problem (q "Which of the following is correct?")
                       (s "zulu")
                       (o "zebra")
                       (o "zap")
                       (o "zulu"))))

(define introduction
  (tutorial 'introduction
            #:name "Introduction to the ICAO Spelling Alphabet"
            #:synopsis "A general overview of the spelling alphabet
and its uses."
            #:chapters
            (list
             (chapter "General overview"
                      (list
                       "The NATO phonetic alphabet, more accurately
known as the International Radiotelephony Spelling Alphabet and also
called the ICAO phonetic or ICAO spelling alphabet, as well as the ITU
phonetic alphabet, is the most widely used spelling alphabet. Although
often called \"phonetic alphabets\", spelling alphabets are not
associated with phonetic transcription systems such as the
International Phonetic Alphabet. Instead, the International Civil
Aviation Organization (ICAO) alphabet assigned code words
acrophonically to the letters of the English alphabet so that critical
combinations of letters and numbers can be pronounced and understood
by those who transmit and receive voice messages by radio or telephone
regardless of language barriers or the presence of transmission
static."
                       "The 26 code words in the NATO phonetic
alphabet are assigned to the 26 letters of the English alphabet in
alphabetical order as follows:
Alfa
Bravo
Charlie
Delta
Echo
Foxtrot
Golf
Hotel
India
Juliett
Kilo
Lima
Mike
November
Oscar
Papa
Quebec
Romeo
Sierra
Tango
Uniform
Victor
Whiskey
X-ray
Yankee
Zulu.")))
            #:completion
            (chapter "Introduction Complete!"
                     '("You have finished our introduction.

From now you will be asked to remember which code word belongs to
which letter of the alphabet."))))


;;;; Discipline
;;;
;;; We have defined the exercises above, so we now we can simply define the
;;; discipline's meta data

(define icao-module
  (module
   'icao-spelling
    #:name "ICAO Spelling Alphabet"
    #:version "0.1"
    #:keywords '("language")
    #:synopsis "Learn to use the ICAO / NATO spelling alphabet for
spelling words."
    #:description "The NATO phonetic alphabet, more accurately known
as the International Radiotelephony Spelling Alphabet and also called
the ICAO phonetic or ICAO spelling alphabet, as well as the ITU
phonetic alphabet, is the most widely used spelling alphabet. Although
often called \"phonetic alphabets\", spelling alphabets are not
associated with phonetic transcription systems such as the
International Phonetic Alphabet. Instead, the International Civil
Aviation Organization (ICAO) alphabet assigned code words
acrophonically to the letters of the English alphabet so that critical
combinations of letters and numbers can be pronounced and understood
by those who transmit and receive voice messages by radio or telephone
regardless of language barriers or the presence of transmission
static."
    #:creator "Alex Sassmannshausen"
    #:contents
    (list introduction alfa bravo charlie delta echo foxtrot golf
          hotel india juliett kilo lima mike november oscar papa
          quebec romeo sierra tango uniform victor whiskey x-ray
          yankee zulu)
    #:attribution
    (list
     (media
      #:urls
      (list "http://en.wikipedia.org/wiki/ICAO_spelling_alphabet")))
    #:resources
    (list
     (media
      #:urls
      (list "http://en.wikipedia.org/wiki/ICAO_spelling_alphabet"
            "http://en.wikipedia.org/wiki/Book:NATO_phonetic_alphabet")))))

;;; spelling-alphabet.scm ends here
