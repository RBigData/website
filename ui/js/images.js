function banner() { };
  ut = new banner();
  i = 0
  ut[i++]= "<img title='ut' src='ui/img/ut.png' alt='' width=147 height=52>"
  ut[i++]= "<img title='nsf' src='ui/img/nsf.png' alt='' width=50 height=52>"
  ut[i++]= "<img title='xsede' src='ui/img/xsede-2.png' alt='' width=158 height=52>"
  
  ornl = new banner();
  i = 0
  ornl[i++]= "<img title='ornl' src='ui/img/ornl.png' alt='' width=133 height=52>"
  ornl[i++]= "<img title='doe' src='ui/img/doe.png' alt='' width=51 height=52>"
  ornl[i++]= "<img title='olcf' src='ui/img/olcf.png' alt='' width=198 height=52>"
  
  n=Math.random();
  
  document.write("<div align=center style='line-height:0px'>")
  document.write("<img title='pbdR' src='ui/img/newpbdr.png' alt='' height=75>")
  document.write("</div><div align=center style='line-height:0px'>")
  // this is disgusting, but I don't actually know javascript so I can't make this acceptable
  
  numpics=3;
  
  if (n < 0.7) // slight bias for UT
  {
    for (i=0; i<numpics; i++){
      document.write( ut[i] );
    }
    document.write("</div><div align=center style='line-height:0px'>")
    for (i=0; i<numpics; i++){
      document.write( ornl[i] );
    }
  }
  else
  {
    for (i=0; i<numpics; i++){
      document.write( ornl[i] );
    }
    document.write("</div><div align=center style='line-height:0px'>")
    for (i=0; i<numpics; i++){
      document.write( ut[i] );
    }
  
  document.write('</div>')
}
