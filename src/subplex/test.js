let Main = require('./subplex.js');

// console.log("---calcc",Main.calcc(1,[-1,0,1,0].dArray(1,1,4),2,1,false,[0,1].dArray()))
// console.log("---dasum", Main.dasum(2,[-2,4].dArray(),1))
// console.log("---daxpy",Main.daxpy(2,-1,[2,4].dArray(),1, [2,2].dArray(),1))
// console.log("---dcopy",Main.dcopy(2,[1].dArray(),0, [2,2].dArray(2),1))
// console.log("---dist",Main.dist(2,[2,4].dArray(),[2,2].dArray()))
// console.log("---dscal",Main.dscal(2,2,[2,4].dArray(),1))

// // var evalfout = {};
// // console.log(Main.evalf((n,x) => x.length * n ,3,[1,2,3],[1,2,3],2,[1,2,3], evalfout,0)) //amir
 
// console.log("---fstats",Main.fstats(2,0, true))                    // 0 [ 2, 2, 2, 0 ]
// console.log("---newpt",Main.newpt(2,2,[1,2].dArray(),[1,-2].dArray(),true,[1,1].dArray(),true))
// console.log("---order",Main.order(2,[1,2].dArray(),[1].dArray(),[].dArray(),[].dArray()))
// // // console.log(Main.partx(2,1,[2,4],1, [2,2],1))
// // // console.log(Main.setstp(2,1,[2,4],1, [2,2],1))
// // // console.log(Main.simplx(2,1,[2,4],1, [2,2],1))
// console.log("---sortd",Main.sortd(3,[2,-4, 7].dArray(),[1,2,3].dArray()))   
// // // console.log(Main.start(2,1,[2,4],1, [2,2],1))
// console.log(Main.subopt(2)) /** ok */
// // // console.log(Main.subplex(2,1,[2,4],1, [2,2],1))
// // // console.log(Main.subplx(2,1,[2,4],1, [2,2],1))



// n = 10;
// tol1 = 0.1;
// tol2  = 1e-4;
// tol3 = 0.1;
// nf1 = 100;
// nf2 = 1000;
// nf3 = 100;
// scl = 0.1
// x = new Array(10).fill(0.1);
// niwmax = nwmax = nxmax = 300;
// scale[1] = - Math.abs(scl);
// tol = tol1;
// maxnfe = nf1;
// mdcont = 0;
// mduser = 0;
// mdsing = 0;
// mode = 4 * mdsing + 2 * mduser + mdcont;

Main.f = function (n,x) {  
   let x1 = x[1]
   let x2 = x[2]
  return 100*(x2-x1*x1)**2+(1-x1)**2

}
Main.x0 = [11,-33];
Main.run()

console.log('aaaaaaaaa')