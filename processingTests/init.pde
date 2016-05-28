#lang processing

float fmax = 0.79;
float fmin = 0.69;
float da = PI/6;
float db = PI/5; 

void tree(float x, float y, float len, float ang) {
  float x2 = x + len * cos(ang);
  float y2 = y + len * sin(ang);

  line(xyz(x, y, 0), xyz(x2, y2, 0));
  if (len < 10)
     len = 1;
  else
     len = 2;
  if (len < 10) 
    line(xyz(x, y, 0), xyz(x2, y2, 0));
  else {
    tree(x2, y2, random(fmin,fmax) * len, ang + da);
    tree(x2, y2, random(fmin,fmax) * len, ang - db);
  }                 
}

/* (#<syntax:1:0 module> #<syntax:1:0 anonymous-module> #<syntax:1:6 processing/lang/processing> .
    #<syntax:1:0 ((#%module-begin (p-declaration (fmax 0.79)) (p-declaration (fmin 0.69)) (p-declaration (da (p-div PI 6.0))) (p-declaration (db (p-div PI 5.0)))
                    (p-function (tree-FFFF-fn x y len ang)
                      (p-call #:call xyz-FFF-fn x2 y2 0))
                    (p-if (p-lt len 10)
                      (p-assignment p-assign (p-left-value len #:name) 1)
                      (p-assignment p-assign (p-left-value len #:name) 2))
                    (p-if (p-lt len 10) (p-call #:call line-OO-fn (p-call #:call xyz-FFF-fn x y 0)
                                          (p-call #:call xyz-FFF-fn x2 y2 0))
                      (p-block (p-call #:call tree-FFFF-fn x2 y2 (p-mul (p-call #:call random-FF-fn fmin fmax) len) (p-add ang da))
                        (p-call #:call tree-FFFF-fn x2 y2 (p-mul (p-call #:call random-FF-fn fmin fmax) len) (p-sub ang db)))))))
(p-initialize)))>)

*/