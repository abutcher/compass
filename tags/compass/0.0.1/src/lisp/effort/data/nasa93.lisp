(defun nasa93 ()
  (data
   :name 'nasa93
   :columns '($RELY $DATA $CPLX $TIME $STOR $VIRT $TURN $ACAP $AEXP $PCAP $VEXP $LEXP $MODP $TOOL $SCED $LOC $ACT_EFFORT)
   :egs '((1.15 0.94 1.15 1 1 0.87 0.87 1 1 1 1 0.95 0.91 1 1.08 25.9 117.6)
	  (1.15 0.94 1.15 1 1 0.87 0.87 1 1 1 1 0.95 0.91 1 1.08 24.6 117.6)
	  (1.15 0.94 1.15 1 1 0.87 0.87 1 1 1 1 0.95 0.91 1 1.08 7.7 31.2)
	  (1.15 0.94 1.15 1 1 0.87 0.87 1 1 1 1 0.95 0.91 1 1.08 8.2 36)
	  (1.15 0.94 1.15 1 1 0.87 0.87 1 1 1 1 0.95 0.91 1 1.08 9.7 25.2)
	  (1.15 0.94 1.15 1 1 0.87 0.87 1 1 1 1 0.95 0.91 1 1.08 2.2 8.4)
	  (1.15 0.94 1.15 1 1 0.87 0.87 1 1 1 1 0.95 0.91 1 1.08 3.5 10.8)
	  (1.15 0.94 1.15 1 1 0.87 0.87 1 1 1 1 0.95 0.91 1 1.08 66.6 352.8)
	  (1.15 0.94 1.15 1.66 1.56 0.87 1.07 0.86 0.91 0.86 1 0.95 0.91 0.91 1 7.5 72)
	  (1 0.94 1.15 1 1 0.87 0.87 0.86 0.82 0.7 1 0.95 1 1 1 20 72)
	  (1 0.94 1.15 1 1 0.87 0.87 0.86 0.82 0.86 1 0.95 1 1 1 6 24)
	  (1 0.94 1.15 1 1 0.87 0.87 0.86 0.82 0.7 1 0.95 1 1 1 100 360)
	  (1 0.94 1.15 1 1 0.87 0.87 0.86 0.82 1 1 1.07 1 1 1 11.3 36)
	  (1 0.94 1.15 1 1 1.15 0.87 0.86 0.91 0.86 1.1 1.14 1 1 1 100 215)
	  (1 0.94 1.15 1 1 0.87 0.87 0.86 0.82 0.86 1 0.95 1 1 1 20 48)
	  (1 0.94 1.15 1 1 0.87 0.87 0.86 1 1 1 1.14 1 1 1 100 360)
	  (1 0.94 1.15 1 1.56 0.87 0.87 0.86 0.82 0.7 1 0.95 1 1 1 150 324)
	  (1 0.94 1.15 1 1 0.87 0.87 0.86 0.91 0.86 1 0.95 1 1 1 31.5 60)
	  (1 0.94 1.15 1 1 0.87 0.87 0.86 0.82 0.86 1 0.95 1 1 1 15 48)
	  (1 0.94 1.15 1 1.56 0.87 0.87 0.86 0.91 1 1 0.95 1 1 1 32.5 60)
	  (1.15 0.94 1.15 1 1 0.87 0.87 1 1 1 1 0.95 0.91 1 1.08 19.7 60)
	  (1.15 0.94 1.15 1 1 0.87 0.87 1 1 1 1 0.95 0.91 1 1.08 66.6 300)
	  (1.15 0.94 1.15 1 1 0.87 0.87 1 1 1 1 0.95 0.91 1 1.08 29.5 120)
	  (1.15 1 1 1.11 1 1 1 1 0.91 0.86 1 1 1 1 1 15 90)
	  (1.15 1 1.15 1 1 1 1 1 0.91 0.86 1 1 1 1 1 38 210)
	  (1 1 1 1 1 1 1 1 0.91 0.86 1 1 1 1 1 10 48)
	  (1 1.16 1.15 1.3 1.21 0.87 1.07 0.71 0.91 1 1.1 0.95 0.82 0.83 1.08 15.4 70)
	  (1 1.16 1.15 1.3 1.21 0.87 1.07 0.71 0.91 1 1.1 0.95 0.82 0.83 1.08 48.5 239)
	  (1 1.16 1.15 1.3 1.21 0.87 1.07 0.71 0.91 1 1.1 0.95 0.82 0.83 1.08 16.3 82)
	  (1 1.16 1.15 1.3 1.21 0.87 1.07 0.71 0.91 1 1.1 0.95 0.82 0.83 1.08 12.8 62)
	  (1 1.16 1.15 1.3 1.21 0.87 1.07 0.71 0.91 1 1.1 0.95 0.82 0.83 1.08 32.6 170)
	  (1 1.16 1.15 1.3 1.21 0.87 1.07 0.71 0.91 1 1.1 0.95 0.82 0.83 1.08 35.5 192)
	  (1.15 0.94 1.15 1 1 0.87 0.87 1 1 1 1 0.95 0.91 1 1.08 5.5 18)
	  (1.15 0.94 1.15 1 1 0.87 0.87 1 1 1 1 0.95 0.91 1 1.08 10.4 50)
	  (1.15 0.94 1.15 1 1 0.87 0.87 1 1 1 1 0.95 0.91 1 1.08 14 60)
	  (1.15 1 1.15 1 1 1 1 1 1 1 1 1 1 1 1 6.5 42)
	  (1 1 1.15 1 1 1 1 1 1 1 1 1 1 1 1 13 60)
	  (1 1 1.15 1 1 1 1 1 1 0.86 1 0.95 0.91 0.91 1 90 444)
	  (1 1 1.15 1 1 1 1 1 1 1 1 1 1 1 1 8 42)
	  (1 1 1.15 1.11 1 1 1 1 1 1 1 1 1 1 1 16 114)
	  (1 1.08 1.15 1.3 1.06 0.87 1.07 0.86 1 0.86 1.1 0.95 0.91 1 1.08 177.9 1248)
	  (1.15 0.94 1.15 1 1 0.87 0.87 1 1 0.86 1 1 0.91 1.24 1 302 2400)
	  (1 1.08 0.85 1 1 1.15 1 0.86 0.91 1 1 1 0.91 0.91 1 282.1 1368)
	  (1.15 1.08 0.85 1 1 1 1.07 0.86 0.91 1 1 1 0.91 1 1 284.7 973)
	  (1.15 1.08 1 1 1 0.87 0.87 1 0.91 0.86 1 0.95 1 1 1 79 400)
	  (0.88 1 1 1 1 0.87 0.87 0.86 0.91 0.7 1 0.95 1.1 1.1 1.04 423 2400)
	  (1 1 1 1 1 0.87 1 0.86 0.82 0.7 1.1 0.95 0.91 1 1 190 420)
	  (1 1 1.15 1 1.06 1 1 0.86 0.91 1 1 0.95 0.91 1 1.04 47.5 252)
	  (1.4 1 1.65 1.11 1.06 0.87 0.87 1 0.91 1 1 1 1.1 0.91 1 21 107)
	  (1 1.08 1.15 1.3 1 1 1.07 0.86 0.91 0.86 1 0.95 1.1 1.1 1.04 78 571.4)
	  (1 1.08 1.15 1.3 1 1 1.07 0.86 0.91 0.86 1 0.95 1.1 1.1 1.04 11.4 98.8)
	  (1 1.08 1.15 1.3 1 1 1.07 0.86 0.91 0.86 1 0.95 1.1 1.1 1.04 19.3 155)
	  (1.15 1 1.3 1.11 1.06 0.87 1.07 0.86 1 1 0.9 0.95 1.1 0.83 1.04 101 750)
	  (1.15 1 1.15 1.11 1.06 0.87 1.07 1 0.91 1 1 1 1.1 0.83 1 219 2120)
	  (1.15 1 1.15 1.11 1.06 0.87 1.07 1 0.91 1 1 1 1.1 0.83 1 50 370)
	  (1.4 1.08 1.15 1.3 1.21 1 1 0.71 0.82 0.7 1 0.95 0.91 0.91 1.08 227 1181)
	  (1 1.08 1.3 1 1 0.87 1 0.86 1 0.7 1.1 1 0.91 1 1.08 70 278)
	  (1.15 0.94 1.15 1 1 0.87 0.87 1 1 1 1 0.95 0.91 1 1.08 0.9 8.4)
	  (1.4 0.94 1.65 1.66 1.21 0.87 0.87 0.86 0.82 0.86 1.21 0.95 1.24 1.24 1.04 980 4560)
	  (1 0.94 1.15 1 1 0.87 0.87 0.71 1 0.7 0.9 0.95 1 1.1 1 350 720)
	  (1.15 1 1.65 1.11 1.06 0.87 0.87 0.86 1 1 0.9 0.95 0.91 0.91 1 70 458)
	  (1.15 1 1.65 1.11 1.06 0.87 0.87 0.86 1 1 0.9 0.95 0.91 0.91 1 271 2460)
	  (1 1 1 1 1 0.87 0.87 0.86 0.91 0.86 1 0.95 1 1.1 1 90 162)
	  (1 1 1 1 1 0.87 0.87 0.86 0.91 0.86 1 0.95 1 1.1 1 40 150)
	  (1.15 1 1.15 1.11 1 0.87 0.87 0.86 0.91 0.86 1 0.95 1 1 1 137 636)
	  (1.15 1 1.15 1.11 1 1.15 0.87 0.86 0.91 0.86 1 0.95 1 1.24 1 150 882)
	  (1.4 1 1.15 1.11 1 0.87 0.87 0.86 0.91 0.86 1 0.95 1 1 1 339 444)
	  (0.88 1.08 0.85 1 1 1.15 0.87 0.86 0.91 0.86 1 0.95 1 1.1 1 240 192)
	  (1.15 1 1.15 1 1.21 0.87 1 0.86 0.91 0.86 0.9 0.95 1.1 1.1 1.08 144 576)
	  (1 0.94 1 1 1.21 0.87 1 0.86 0.91 0.86 0.9 0.95 1.1 1.1 1.08 151 432)
	  (1 0.94 1.15 1 1.21 0.87 1 0.86 0.91 0.86 0.9 0.95 1.1 1.1 1.08 34 72)
	  (1 1 1.15 1 1.21 0.87 1 0.86 0.91 0.86 0.9 0.95 1.1 1.1 1.08 98 300)
	  (1 1 1.15 1 1.21 0.87 1 0.86 0.91 0.86 0.9 0.95 1.1 1.1 1.08 85 300)
	  (1 0.94 1 1 1.21 0.87 1 0.86 0.91 0.86 0.9 0.95 1.1 1.1 1.08 20 240)
	  (1 0.94 1 1 1.21 0.87 1 0.86 0.91 0.86 0.9 0.95 1.1 1.1 1.08 111 600)
	  (1.15 1.16 1.15 1 1.21 0.87 1 0.86 0.91 0.86 0.9 0.95 1.1 1.1 1.08 162 756)
	  (1.15 1.08 1.3 1 1.21 0.87 1 0.86 0.91 0.86 0.9 0.95 1.1 1.1 1.08 352 1200)
	  (1.15 1 1.3 1 1.21 0.87 1 0.86 0.91 0.86 0.9 0.95 1.1 1.1 1.08 165 97)
	  (1.15 1 1.3 1.11 1.06 0.87 1.15 0.86 1 1 0.9 0.95 0.91 0.83 1.04 60 409)
	  (1.15 1 1.3 1.11 1.06 0.87 1.15 0.86 1 1 0.9 0.95 0.91 0.83 1.04 100 703)
	  (1.15 1.16 1.3 1.66 1.56 1.15 1.07 1 1 1 1.1 1.07 1 1 1.04 32 1350)
	  (1.15 1.08 1.15 1.3 1.56 1.15 1.07 0.86 0.91 0.86 0.9 0.95 0.91 1 1 53 480)
	  (1.15 0.94 1.3 1.3 1.56 0.87 1 0.71 0.82 0.7 1.21 1.14 0.91 0.91 1 41 599)
	  (1.15 0.94 1.3 1.3 1.56 0.87 1 0.71 0.82 0.7 1.21 1.14 0.91 0.91 1 24 430)
	  (1.4 1.08 1.3 1.66 1.56 1 1 0.86 0.91 0.86 0.9 0.95 0.91 1 1.04 165 4178.2)
	  (1.4 1.08 1.3 1.66 1.56 1 1 0.86 0.91 0.86 0.9 0.95 0.91 1 1.04 65 1772.5)
	  (1.4 1.08 1.3 1.66 1.56 1 0.87 0.86 0.91 0.86 0.9 0.95 0.91 1 1.04 70 1645.9)
	  (1.4 1.08 1.65 1.66 1.56 1 1 0.86 0.91 0.86 0.9 0.95 0.91 1 1.04 50 1924.5)
	  (1.4 0.94 1.3 1.3 1.56 0.87 0.87 0.86 1.13 1 1.21 1.07 1.1 0.91 1.04 7.25 648)
	  (1.4 1.08 1.3 1.66 1.56 1 1 0.86 0.91 0.86 0.9 0.95 0.91 1 1.04 233 8211)
	  (1.15 1 1.3 1.3 1.21 1.15 1.07 1 1 1 1.1 1.07 1 1 1.04 16.3 480)
	  (1.15 1 1.3 1.3 1.21 1.15 1.07 1 1 1 1.1 1.07 1 1 1.04 6.2 12)
	  (1.15 1 1.3 1.3 1.21 1.15 1.07 1 1 1 1.1 1.07 1 1 1.04 3 38)
	  )))