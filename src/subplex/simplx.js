let simplx = function(f,n,step,ns,ips,maxnfe,cmode,x,fx,nfe,s,fs,iflag){
    let i,icent,ih = [].dArray(),il = [].dArray(),inew = [].dArray(),is = [].dArray(),itemp,j,npts
    let dist,dum = [].dArray(),fc = [].dArray(),fe = [].dArray(),fr = [].dArray(),tol
    let small = [].dArray(),updatc = [].dArray()
    s = s.clone(1,ns,ns+3);
    let goto_variable = 0;
    while (true)
    {
      switch (goto_variable){
         case 0:
            if (cmode){
               goto_variable = 50;
            }
            else{
               goto_variable = 5;
            }
         break;
         case 5:
            npts = ns+1
            icent = ns+2
            itemp = ns+3
            updatc[0] = false
            this.start(n,x,step,ns,ips,s.clone(1,ns,ns+3),small)
            if (small[0]){
               iflag = 1
               return
            }
            fs[1] = fx[0]
            for(j = 2 ; j<= npts; j++){// 10
               this.evalf (f,ns,ips,s.clone((j-1)*ns+1),n,x,fs.clone(j),this.nfe)
            }

            il[0] = 1
            this.order(npts,fs,il,is,ih)
            this.tol = this.psi*this.dist(ns,s.clone((ih[0]-1)*ns+1),s.clone((il[0]-1)*ns+1))
            goto_variable = 20;
         break;
         case 20: // 20
            this.calcc (ns,s,ih,inew,updatc,s.clone((icent-1)*ns+1))
            updatc[0] = true
            inew[0] = ih[0]
            this.newpt(ns,this.alpha,s.clone((icent-1)*ns+1),s.clone((ih[0]-1)*ns+1),true,s.clone((itemp-1)*ns+1),small)
            goto_variable = 40; // 40
            if (small[0]){
               break;
            }

            this.evalf (f,ns,ips,s.clone((itemp-1)*ns+1),n,x,fr,this.nfe)
            if (fr[0] < fs[il[0]]){  
               this.newpt(ns,-this.gamma,s.clone((icent-1)*ns+1),s.clone((itemp-1)*ns+1),true,s.clone((ih[0]-1)*ns+1),small)
               goto_variable = 40; // 40
               if (small[0]){
                  break;
               }
               this.evalf (f,ns,ips,s.clone((ih[0]-1)*ns+1),n,x,fe,this.nfe)
               if (fe[0] < fr[0]){
                  fs[ih[0]] = fe[0]
               }
               else{
                  this.dcopy(ns,s.clone((itemp-1)*ns+1),1,s.clone((ih[0]-1)*ns+1),1)
                  fs[ih[0]] = fr[0]
               }
            }
            else if (fr[0] < fs[is[0]]){
               this.dcopy(ns,s.clone((itemp-1)*ns+1),1,s.clone((ih[0]-1)*ns+1),1)
               fs[ih[0]] = fr[0]
            }
            else{
               if (fr[0] > fs[ih[0]]){
                  this.newpt(ns,-this.beta,s.clone((icent-1)*ns+1),s.clone((ih[0]-1)*ns+1),true,s.clone((itemp-1)*ns+1),small)
               }
               else{
                  this.newpt(ns,-this.beta,s.clone((icent-1)*ns+1),s.clone((itemp-1)*ns+1),false,dum,small)
               }     
               goto_variable = 40; // 40
               if (small[0]){
                  break;
               }
               this.evalf (f,ns,ips,s.clone((itemp-1)*ns+1),n,x,fc,this.nfe)
               if (fc[0] < Math.min(fr[0],fs[ih[0]])){
                  this.dcopy(ns,s.clone((itemp-1)*ns+1),1,s.clone((ih[0]-1)*ns+1),1)
                  fs[ih[0]] = fc[0]
               }
               else{
                     for(j = 1; j<= npts; j++){          
                        if (j !== il[0]){
                           this.newpt(ns,-this.delta,s.clone((il[0]-1)*ns+1),s.clone((j-1)*ns+1),false,dum,small)
                           goto_variable = 40; // 40
                           if (small[0]){
                              break;
                           }
                           this.evalf(f,ns,ips,s.clone((j-1)*ns+1),n,x,fs.clone(j),this.nfe)
                        }
                     }
               }   
               updatc[0] = false
            }
            this.order(npts,fs,il,is,ih)
            goto_variable = 40;
         break;
         case 40: //40   continue
            fx[0] = fs[il[0]]
            goto_variable = 50;
         break;
         case 50: // 50   continue
            if (this.nfe >= maxnfe){
               iflag = -1
            }
            else if (this.dist(ns,s.clone((ih[0]-1)*ns+1),s.clone((il[0]-1)*ns+1)) <= this.tol || small[0]){
               iflag = 0
            }
            else{
               goto_variable = 20;//    go to 20
               break;
            }

            for(i= 1 ; i<= ns; i++){
               x[ips[i]] = s[i][il[0]]
            }
            return
         break;
      }
   }
}

module.exports = simplx;