#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"Brian Yee byee4"

"from cmdline.st"
execname := '$RCSfile: cmdline.st,v $' =~ '.*: (.*),v.*' at: 1.
nl := Character nl.
priorityqueue := SortedCollection new.

"needed for -t flag"
Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

"outbits.st"
"needed for compression if there is an output file"
"did not modify"
Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
         myStream nextPutByte: currentByte.
         self clearByte.
      
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

"bitstream.st"
"reads in bits from inputfileew"
"did not modify"
"needed for uncompression"
Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

"charfreq.st"
"did not modify"
Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

"tree.st"
"modified to add isLeaf boolean and postorder"
Object subclass: Leaf [
   |char count isLeaf|
   char [ ^ char ]
   count [ ^ count ]
   isLeaf [ ^ isLeaf ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
      isLeaf := true.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   depthFirst: visitor prefix: string [
      visitor value: char value: string.
   ]

   postorder: visitor [
      visitor value: char value: isLeaf.
   ]

].

"modified to add isLeaf boolean and postorder"
Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
      isLeaf := false.
   ]

   left [ ^ left]
   right [^ right]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   depthFirst: visitor prefix: string [
      left depthFirst: visitor prefix: string, '0'.
      right depthFirst: visitor prefix: string, '1'.
   ]

   postorder: visitor[
      left postorder: visitor.
      right postorder: visitor.
      visitor value: char value:isLeaf.
   ]

].

"function main"

|inputfile inputfile2 outputfile counter|
counter := 0.
dFlag := false.
tFlag := false.
cFlag := false.
uFlag := false.

"part a"
"create frequency table and entry 256 with count 1 for EOF"
freqtable := ZeroArray new: 257.
freqtable at:256 put:1.

"cmdline.st line 33"
"reads in command line"
Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg |
   (opt = $d) ifTrue: [dFlag := true.].
   (opt = $t) ifTrue: [tFlag := true.].
   (opt = $c) ifTrue: [cFlag := true.].
   (opt = $u) ifTrue: [uFlag := true.].

   "from catfile.st"
   (arg = nil) ifFalse: [
      (counter = 0) ifTrue: [
         "read the input file twice"
         inputfile := FileStream open: arg
                     mode: FileStream read.
         inputfile2 := FileStream open: arg
                     mode: FileStream read.
      ]
      ifFalse: [
        outputfile1 := (Smalltalk getArgv:3).
        outputfile := FileStream open: outputfile1
                      mode: FileStream write.
      ].
      counter := counter + 1.
   ].
].

|tree|
uFlag ifFalse:[
   "256 or 257?"
   encodeTable := ZeroArray new: 257.

   "modify charfreq.st"
   "part b"
   "iterate over the freq table"
   [inputfile atEnd not] whileTrue: [
      |ordChar|
      ordChar := inputfile next asInteger.
      freqtable incr: ordChar.
   ].

   "create a leaf node and insert that leaf node"
   "into a priority queue with the character and count"
   freqtable keysAndValuesDo: [:key :value |
      (value notNil and: [value > 0]) ifTrue: [
         priorityqueue add: (Leaf new: key count: value).
      ]
   ].

   "repeeatedly remove the two smallest elements"
   "from the priority queue creating a new tree which is then"
   "entered into the priority queue"
   "stops when there is only 1 tree left"
   [priorityqueue size = 1] whileFalse:[
      |leftchild rightchild|
      leftchild := priorityqueue removeFirst.
      rightchild := priorityqueue removeFirst.
      subtree := (Tree new: (leftchild char) 
      count:(leftchild count)+(rightchild count)
      left: leftchild right: rightchild).
      priorityqueue add: subtree.
   ].

   "tree holds the entirely built tree"
   tree := priorityqueue removeFirst.

   "tree.st"
   "https://www.gnu.org/software/smalltalk/manual/html_node/Arrays.html"
   "part d encoding string path of root to leaf"
   "just dfs from tree.st example"
   tree depthFirst: [:char :string|
      encodeTable at: char put: string.
   ] prefix: ''.

   postordertree := ''.

   "part f"
   "perform a post-order traveresal of the decoding tree"
   "for each life, write out a 0 bit,"
   "followed by the 8bits of the corresponding byte"
   "write highest bit first"
   "if byte 0 write out bit 8 = value 0"
   "1 for byte value of 256  = EOF"
   tree postorder: [:char :isLeaf|
      isLeaf ifFalse:[postordertree := postordertree,'1'.]
      ifTrue:[
         binary := ''.
         "check if byte value is 256 EOF"
         char = 256 ifTrue:[binary := '000000001']
         ifFalse:[
            "check if byte value is 0"
            char=0 ifTrue:[binary := '000000000']
            ifFalse:[
               "write out the bits in the order bit8..."
               8 to:1 by: -1 do:[:i|
                  binary := binary,((char bitAt:i)printString)
               ]
            ].
         ]. 
         "bin should hold the last 8 bits with the 9th bit as 0"
         "used in compression section"
         postordertree := (postordertree, '0', binary).
      ].
   ].

  "debugger"
  "inspects the priorityqueue and prints out the postordertree"
  dFlag ifTrue:[
    priorityqueue inspect.
    stdout << postordertree << nl
  ].

  "this is from charfreq.st"
  "prints out the frequency table"
  "and also prints out the decoding tree"
  tFlag ifTrue:[
    stdout << 'Frequency' << nl.
    freqtable keysAndValuesDo: [:key :value |
       (value notNil and: [value > 0]) ifTrue: [
          stdout << 'freq[' << key asCharacter visible << ']=' << value
                 << nl.
       ]
    ].
    stdout << nl.
    stdout << 'Decoding Tree' << nl.
    encodeTable keysAndValuesDo: [:key :value |
       (value notNil and: [value > 0]) ifTrue: [
          stdout << (encodeTable at:key) << ' '<<nl
       ]
    ].
  ].

"OUTBYTES is our output file"
"cFlag for compression is true"
"believe it works? but not very sure"
  cFlag ifTrue:[
    |EOF|
    "if there is an outputfile"
    "we write to the file"
    (outputfile ~= nil) ifTrue:[
      "from outbits.st"
      "for each interior node write out a 1 bit"
      outBytes := OutBits new: outputfile.
      1 to: (postordertree size) by: 1 do: [:index |
        bit := ((postordertree at: index) asInteger).
        outBytes writeBit: bit.
      ].
      "this is writing out a 1 bit to indicate the end of the tree"
      outBytes writeBit: 1.
      "reopen the input file and for each byte read from the input file"
      "write the encoded version of each byte to the output file"
      [inputfile2 atEnd not] whileTrue:[
        |ordChar|
        ordChar := inputfile2 next asInteger.
        code := (encodeTable at: ordChar).
        1 to: (code size) by: 1 do: [:index |
          bit := ((code at: index) asInteger).
          outBytes writeBit: bit.
        ].
      ].
      "write out the encoding string for EOF"
      "go to EOF in the codetable"
      "then write out its bits"
      EOF := encodeTable at: 256.
      1 to: (EOF size) by: 1 do:[:index|
        bit:= ((EOF at: index)asInteger).
        outBytes writeBit: bit.
      ].
        "this is to pad with 0 bits"
        "saw on piazza"
        outBytes flushByte.
    ]
    "if there is no output file in command line"
    ifFalse:[
      "each interior node write out a bit"
      stdout <<postordertree.
      "write 1 bit to indicate end of tree"
      stdout<<1. 
      "read input file and write encoded version of each byte"
      "just need to get to the index or the encodeTable"
      "and put that into stdout"
      [inputfile2 atEnd not] whileTrue:[
        |ordChar|
        ordChar := inputfile2 next asInteger.
        code := (encodeTable at: ordChar).
        stdout <<code.
      ].
      "get the eof encoded string and put to std"
      eof := (encodeTable at: 256).
      stdout<<eof<<nl.
    ].
  ].
].

"decompression flag is true"
(uFlag) ifTrue:[
  |EOF outBytes stack|
  EOF:=false.
  outBytes := OutBits new: outputfile.  
  stack := OrderedCollection new.
  input := BitStream new: inputfile.
  finished:=false.
  reconstruct:=''.
  currentnode:=''.   
   
  "Reconstructing the huffman decoding tree"
  "read in 1 bit"
  [(input atEnd not) & (EOF not)] whileTrue: [
    "treenode has the 1 bit being read"
    treenode := input nextBit.
    leafchar := 0.
    finished ifFalse:[
      "if the bit read is a 0"
      "read the next 8 bits and reconstruct the byte"
      treenode ~= 1 ifTrue:[
        8 to:1 by:-1 do:[:i|
          bit := input nextBit.      
          leafchar := leafchar bitAt:i put:bit.
        ].
        "if the value is 0"
        "read in one more bit"
        "and if it is 1, encode EOF"
        leafchar = 0 ifTrue:[
          checkforone := input nextBit.
          checkforone = 1 ifTrue:[
            leafchar := leafchar bitAt:9 put:1.
          ].
        ]. 
        "push the byte onto the stack"
        stack add: (Leaf new: leafchar count: '').
      ].

      "if the bit read is a 1" 
      treenode = 1 ifTrue:[
        "pop the 1-subtree from the stack"
        "then pop 0-subtree and create a new tree"
        "1-subtree = rightchild 0-subtree = leftchild"
        stack size > 1 ifTrue:[
          rightchild := stack removeLast.
          leftchild := stack removeLast.   
          newtree := (Tree new: -1 count: '' 
          left: leftchild right: rightchild).
          "put the tree back on the stack"
          stack add: newtree.
        ]
        "if the stack size = 1"
        "popping this will be the decoding tree" 
        "finished = true because finish building decoding tree"
        ifFalse:[
          finished := true.   
          "reconstruct = final tree"
          reconstruct := stack removeLast.
          "currentnode = final tree will be reset pointer"
          "for later on"
          currentnode:=reconstruct.
        ].
      ].
    ]
    "encodetree is FINISHED"
    "now loop over rest of input file to reconstruct the original file"
    "currentnode = pointer back to the root of the tree"
    ifTrue:[
      currentnode isLeaf ifFalse:[
        "going down the 0 link"
        treenode ~= 1 ifTrue:[       
          currentnode := currentnode left.
          "check to see if it is a leaf"
          "if this node is a leaf write out the"
          "corresponding bytes and reset the pointer back to"
          "the root of the tree"
          currentnode isLeaf ifTrue:[
            EOF not ifTrue:[
              "checks to see if EOF to break out and end"  
              ((currentnode char)=256) ifTrue:[
                EOF:=true.
              ] 
              "if node is not EOF write out its bytes"
              ifFalse:[
                "if there is no outputfile argument"
                "output the bytes to the stdout"
                outputfile = nil ifTrue:[
                  stdout nextPut: (Character value: (currentnode char)).
                ]
                "if is an outputfile argument"
                "output to outputfile"
                "same as what we did in compression"
                ifFalse:[
                  8 to: 1 by: -1 do: [:index |
                    bit:= (currentnode char) bitAt: index.
                    outBytes writeBit: bit.
                  ].
                ].
                "reset pointer back to the root of the tree"
                currentnode:=reconstruct.
              ].
            ].
          ]
        ]
        "going down 1-link" 
        "same exact as going down 0-link"
        "check if EOF if not output to stdout if no"
        "outputfile argument. If there is output to"
        "outputfile. Then reset back to the root of the tree"
        ifFalse:[       
          currentnode:=currentnode right.
          currentnode isLeaf ifTrue:[
            EOF not ifTrue:[
              ((currentnode char)=256) ifTrue:[
                EOF:=true.
              ] 
              ifFalse:[
                outputfile=nil ifTrue:[
                  stdout nextPut: (Character value: (currentnode char)).
                ]
                ifFalse:[
                  8 to: 1 by: -1 do: [:index |
                    bit:= (currentnode char) bitAt: index.
                    outBytes writeBit: bit.
                  ].
                ].
                currentnode:=reconstruct.
              ].
            ].
          ]
        ].
      ].
    ].
  ].
]. 








