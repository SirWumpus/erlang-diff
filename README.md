Erlang diff(1)
==============

```
usage: ediff [-d] file1 file2

-d              write edit distance
```

Description
-----------

This is a functioning micro diff tool using an [O(NP) algorithm][Wu+89], compared to the older [O(ND) difference algorithm][Mye86] used by some versions of diff.  Its output is based on the default diff(1) output described by POSIX and [The Open Group Base Specifications][SUSV7].  The output is suitable for use with patch(1).

The -d option simply writes the edit distance between file1 and file2.


References
----------

Wu, Manber, Myers, and Miller; August 1989;  
"An O(NP) Sequence Comparison Algorithm";  
<http://myerslab.mpi-cbg.de/wp-content/uploads/2014/06/np_diff.pdf>
	
Fowler, Noll, Vo; 1994  
<http://www.isthe.com/chongo/tech/comp/fnv/index.html>

Fowler, Noll, Vo on Wikipedia  
<https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function>

"diff", The Open Group Base Specifications Issue 7  
IEEE Std 1003.1, 2013 Edition  
<http://pubs.opengroup.org/onlinepubs/9699919799/utilities/diff.html>

Eugene W. Myers; "An O(ND) Difference Algorithm and Its Variations";  
Algorithmica, 1986, pages. 251-266  
<http://www.xmailserver.org/diff2.pdf>

Webb Miller and Eugene W. Myers; "A File Comparison Program";  
Software-Practice And Experience, Vol. 15(11). 1025-1040 (November 1985)  
<http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.189.70&rep=rep1&type=pdf>

D.S. Hirschberg, "A linear space algorithm for computing maximal common subsequence problem";  
Comm. of ACM, Vol. 18, June 1975, pages 341-343  
<http://www.mathcs.emory.edu/~cheung/Courses/323/Syllabus/DynProg/Docs/Hirschberg-LCS-1975.pdf>

Which hashing algorithm is best for uniqueness and speed?  
<http://programmers.stackexchange.com/questions/49550/which-hashing-algorithm-is-best-for-uniqueness-and-speed>

[Wu+89]: http://myerslab.mpi-cbg.de/wp-content/uploads/2014/06/np_diff.pdf

[FNV94]: http://www.isthe.com/chongo/tech/comp/fnv/index.html

[FNVWi]: https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function

[Mye86]: http://www.xmailserver.org/diff2.pdf

[SUSV7]: http://pubs.opengroup.org/onlinepubs/9699919799/utilities/diff.html

[Mil85]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.189.70&rep=rep1&type=pdf

[Hir75]: http://www.mathcs.emory.edu/~cheung/Courses/323/Syllabus/DynProg/Docs/Hirschberg-LCS-1975.pdf

[HshCmp]: http://programmers.stackexchange.com/questions/49550/which-hashing-algorithm-is-best-for-uniqueness-and-speed


Copyright
---------

Copyright 2017 by Anthony Howe.  All rights reserved.


MIT License
-----------

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
