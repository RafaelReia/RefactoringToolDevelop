#lang processing


boolean value2 =
true && !false;

boolean value =
(1 < 2) || (1 < 3) || true;

boolean value3 =
!(1 > 2);

boolean test3 =
!(1 < 3);

boolean alfa = true;

float teste = 1.0;

float height = 200;
for (int i = 5; i < height; i += 5) {
  stroke(255);   // Set the color to white
  if (i < 35) {  // When 'i' is less than 35...
    stroke(0);   //...set the color to black
  }
  line(30, i, 80, i);
}