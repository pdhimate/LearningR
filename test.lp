/* Objective function */
min: +54 C1 +54 C2 +51 C3 +53 C4 +51 C5 +57 C6 +52 C7 +52 C8 +50 C9 +53 C10 +54 C11 +56 C12 +56 C13 +54 C14
 +55 C15 +53 C16;

/* Constraints */
+C1 +C2 +C3 +C4 = 1;
+C5 +C6 +C7 +C8 = 1;
+C9 +C10 +C11 +C12 = 1;
+C13 +C14 +C15 +C16 = 1;
+C1 +C5 +C9 +C13 = 1;
+C2 +C6 +C10 +C14 = 1;
+C3 +C7 +C11 +C15 = 1;
+C4 +C8 +C12 +C16 = 1;

/* Integer definitions */
int C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,C16;
