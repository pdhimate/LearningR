/* Objective function */
min: +50 C1 +45 C2 +55 C3 +48 C4 +52 C5 +50 C6 +8 C7 +8 C8 +8 C9 +8 C10 +8 C11 +8 C12;

/* Constraints */
+C1 -C7 = 100;
+C2 +C7 -C8 = 250;
+C3 +C8 -C9 = 190;
+C4 +C9 -C10 = 140;
+C5 +C10 -C11 = 220;
+C6 +C11 = 110;
