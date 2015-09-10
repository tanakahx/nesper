# Nesper

Neural network library for Common Lisp

## Usage

A Neural network which learns XOR function can be created like the following.

```lisp
CL-USER> (defparameter *nn* nil)
*NN*
CL-USER> (nesper:setf-network *nn* '(2 2 1) "xor.txt" :input-count 2 :output-count 1 :learning-count 50000)
((#2A((-4.618895519522104d0 -4.6189657253359195d0)
      (3.931726960549626d0 3.9303590822739296d0))
  #2A((-6.559603480966878d0 -6.329716039390059d0)))
 (#2A((1.5791468671103404d0) (-6.168041464078431d0))
  #2A((3.107806112214984d0))))
```

`'(2 2 1)` means that the network has 2 input units, 2 hidden units, and 1 output unit.

`xor.txt` is a text file composed of the list of inputs to the neural network and expected outputs.

```
0 0 0
0 1 1
1 0 1
1 1 0
```

The network can answer the given questions.

```lisp
CL-USER> (nesper:answer *nn* '(0 0))
0.08755272690312033d0
CL-USER> (nesper:answer *nn* '(0 1))
0.9000703821707596d0
CL-USER> (nesper:answer *nn* '(1 0))
0.9000006720134367d0
CL-USER> (nesper:answer *nn* '(1 1))
0.09601144473575372d0
```

## Installation

Put location of this repository into file `~/.config/common-lisp/source-registry.conf` or variable `asdf:*central-registry*` and load with ASDF with `asdf:load-system`.

```console
CL-USER> (asdf:load-system :nesper)
T
```

## Author

* Hiroyuki Tanaka (tanakahx@gmail.com)

## Copyright

Copyright (c) 2015 Hiroyuki Tanaka (tanakahx@gmail.com)

## License

Licensed under the MIT License.
