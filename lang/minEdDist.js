/**
 * Using dynamic programming create the edit distance matrix for two sequences
 * @params s1, s2: the sequences to be compared (as arrays of phonemes)
 * @param insertCost: cost or penalty for insertion...
 * @param deleteCost: ...for deletion...
 * @param subCost: ...for subsititution
 * @return: the matrix and the best total cost
 */
function editM(s1, s2, insertCost, deleteCost, subCost) {
  // Add intial blanks to s1, s2
  s1 = [''].concat(s1);
  s2 = [''].concat(s2);
  
  var n1 = s1.length,
      n2 = s2.length,
      m = [];
  
  // Initialize matrix: n1 x n2
  for (var i = 0; i < n1; i++) {
    m.push([])
    for (var j = 0; j < n2; j++) {
      // For initial row/col, initialize as all insert/delete costs from empty
      if (i == 0) {
        m[i].push([j * insertCost]);
      } else if (j == 0) {
        m[i].push([i * deleteCost]);
      } else {
        // Initialize all other cells as empty
        m[i].push([]);
      }
    }
  }

  // Loop through non-marginal cells and calculate edit distance from states
  // at cells above (del), to the left (ins), and above-left diagonal (sub)
  for (var i = 1; i < n1; i++) {
    var phone1 = s1[i];
    for (var j = 1; j < n2; j++) {
      var phone2 = s2[j];

      var ins = +m[i - 1][j] + insertCost;
      var del = +m[i][j - 1] + deleteCost;
      var sub = +m[i - 1][j - 1] + (s1[i] === s2[j] ? 0 : subCost);

      m[i][j] = Math.min(ins, del, sub);
    }
  }

  return { matrix: m, cost: m[n1 - 1][n2 - 1] };
}

/** 
 * Backtrace algorithm to find min cost sequence:
 * Move from m[n1][n2] back to m[0][0] along the least-cost path.
 * In the case of ties, take the diagonal path.
 * @param editM: the edit distance matrix output, ("matrix") from editM()
 * @return a sequence of strings ("ins", "del" or "sub") indicating the
 *         alignment sequence.
 */
function backtrace(editM) {
  var sequence = [],
    i = editM.length - 1,
    j = editM[0].length -1,
    sub, // backtrace if substitution...
    del, // ...deletion...
    ins; // ...or insertion

  while (i > 0 || j > 0) {
    // each first test if the current cell is at the edge of the matrix and
    // hence if step is possible.  If not, assign null
    sub = ((i > 0 && j > 0) ? editM[i - 1][j - 1] : null);
    del = ((j > 0) ? editM[i][j - 1] : null);
    ins = ((i > 0) ? editM[i - 1][j] : null);

    var bestStep = Math.min(sub, del, ins);
    // Order such that in the envent of a tie, sub is chosen by default
    if (bestStep == sub) {
      sequence.push('sub');
      i--;
      j--;
    } else if (bestStep == del) {
      sequence.push('del');
      j--;
    } else {
      sequence.push('ins');
      i--;
    }
  }

  return sequence.reverse();
}

/**
 * Align two strings according to their minimum edit distance
 * @params s1, s2: "strings" 1 and 2 represented as an array of letters or 
 *                 phones
 * @param sequence: as output by backtrace() above
 */
function align(s1, s2, sequence) {
  var s1Aligned = '',
      s2Aligned = '',
      nextS1 = 0, // index of the next s1 to be added
      nextS2 = 0;
    
  for (var step in sequence) {
    switch (sequence[step]) {
    case 'del':
      s1Aligned += '-';
      s2Aligned += s2[nextS2++];
      break;
    case 'sub':
      s1Aligned += s1[nextS1++];
      s2Aligned += s2[nextS2++];
      break;
    case 'ins':
      s1Aligned += s1[nextS1++];
      s2Aligned += '-';
      break;
    }
  }

  return { s1Aligned: s1Aligned, s2Aligned: s2Aligned }
}

/**
 * This simply combines editM, backtrace, and align in a single function
 * @params str1, str2: input strings
 * @params subC, insC, delC: cost for substitution, insertion, deletion
 * @return an object containing the aligned string sequences, and the total 
 *         cost
 */
function stringAlign(str1, str2, insC, delC, subC) {
  // User input will be a string. Convert to array.
  str1 = str1.split('');
  str2 = str2.split('');
  var em = editM(str1, str2, insC, delC, subC);
  var sequence = backtrace(em.matrix);
  var alignment = align(str1, str2, sequence);
  return { s1Aligned: alignment.s1Aligned,
           s2Aligned: alignment.s2Aligned,
           cost: em.cost }
}


// Define phoneme attributes
var p = [0, 1,   0,   0,   0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, null],
  // aspirated p
  pH  = [0, 1,   0,   0,   0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, null],
  // glottalized p
  pQ  = [0, 1,   0,   0,   0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, null],
  b   = [0, 1,   0,   0,   0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, null],
  t   = [0, 1,   0,   0,   0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, null],
  d   = [0, 1,   0,   0,   0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, null],
  k   = [0, 1,   0,   0,   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, null],
  g   = [0, 1,   0,   0,   0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, null],
  // phi
  PH  = [0, 1,   0,   1,   1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, null],
  // beta
  BH  = [0, 1,   0,   1,   1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, null],
  f   = [0, 1,   0,   1,   1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, null],
  v   = [0, 1,   0,   1,   1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, null],
  // theta
  TH  = [0, 1,   0,   1,   1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, null],
  // eth
  DH  = [0, 1,   0,   1,   1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, null],
  s   = [0, 1,   0,   1,   1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, null],
  z   = [0, 1,   0,   1,   1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, null],
  SH  = [0, 1,   0,   1,   1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, null],
  ZH  = [0, 1,   0,   1,   1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, null], 
  x   = [0, 1,   0,   1,   1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, null],
  // gamma
  GH  = [0, 1,   0,   1,   1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, null],
  tS  = [0, 1,   0,   0,   1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, null],
  dZ  = [0, 1,   0,   0,   1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, null],
  tSH = [0, 1,   0,   0,   1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, null],
  dZH = [0, 1,   0,   0,   1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, null],
  m   = [0, 1,   1,   0,   0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, null],
  MG  = [0, 1,   1,   0,   0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, null],
  n   = [0, 1,   1,   0,   0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, null],
  NY  = [0, 1,   1,   0,   0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, null],
  NG  = [0, 1,   1,   0,   0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, null],
  l   = [0, 1,   1, 0.5, 0.5, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, null],
  LH  = [0, 1,   1,   1,   1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, null],
  // flap r
  RD  = [0, 1,   1,   1,   1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, null],
  y   = [0, 0,   1,   1,   1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, null],
  w   = [0, 0,   1,   1,   1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, null],
  // glottal stop
  Q   = [0, 0, 0.5,   0,   0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, null],
  h   = [0, 0, 0.5,   1,   1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, null],

  // Vowels
  i  = [1, 0, 1, 1, null, null, null, null, 0, 0, 0, 1, null, 0, 1, 0, 0, 0,
        1],
  I  = [1, 0, 1, 1, null, null, null, null, 0, 0, 0, 1, null, 0, 1, 0, 0, 0,
        0],
  e  = [1, 0, 1, 1, null, null, null, null, 0, 0, 0, 1, null, 0, 0, 0, 0, 0,
        1],
  E  = [1, 0, 1, 1, null, null, null, null, 0, 0, 0, 1, null, 0, 0, 0, 0, 0,
        0],
  AE = [1, 0, 1, 1, null, null, null, null, 0, 0, 0, 1, null, 0, 0, 1, 0, 0,
        1],
  // schwa
  aH = [1, 0, 1, 1, null, null, null, null, 0, 0, 0, 1, null, 0, 0, 0, 1, 0,
        1],
  a  = [1, 0, 1, 1, null, null, null, null, 0, 0, 0, 1, null, 0, 0, 1, 1, 0,
        0],
  // broad u
  iU = [1, 0, 1, 1, null, null, null, null, 0, 0, 0, 1, null, 0, 1, 0, 1, 0,
        0.5],
  u  = [1, 0, 1, 1, null, null, null, null, 0, 0, 0, 1, null, 0, 1, 0, 1, 1,
        1],
  // as in hook
  U  = [1, 0, 1, 1, null, null, null, null, 0, 0, 0, 1, null, 0, 1, 0, 1, 1,
        0],
  o  = [1, 0, 1, 1, null, null, null, null, 0, 0, 0, 1, null, 0, 0, 0, 1, 1,
        1],
  // as in ought
  O  = [1, 0, 1, 1, null, null, null, null, 0, 0, 0, 1, null, 0, 0, 0, 1, 1,
        0]; 
  


$(document).ready(function() {
    var string1, string2, minEd;

    $('#edit-button').on('click', function(e) {
        e.preventDefault();
        string1 = $('#word1').val();
        string2 = $('#word2').val();

        minEd = stringAlign(string1, string2, 1, 1, 2);

        $('#word1-aligned').text(minEd.s1Aligned);
        $('#word2-aligned').text(minEd.s2Aligned);
        $('#cost').text(minEd.cost);
    });
   
});
