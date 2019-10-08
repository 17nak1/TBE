let run = function(){
    if(this.scale.length === 1){
        this.scale[0] = -Math.abs(this.scale[0]);
    }
    else{
        for(let i = 0; i< this.scale.length; i++){
            this.scale[i] = Math.abs(this.scale[i]);
        }
    }
    this.n = this.x0.length ;
    this.work = new Array((this.n)*(this.n+6)+1)
    this.iwork = new Array(2*(this.n))

    this.subplx (this.f,this.n,this.tol,this.maxnfe,this.scale.dArray(),this.x0.dArray(),this.fx,this.nfe,this.work.dArray(),this.iwork.dArray(),this.iflag)
    console.log("FX is ===========" , this.fx)
    switch (this.iflag) {
        case -1:
            console.log('number of function evaluations exceeds \'maxit\'');
          break;
        case 0:
            console.log('success! tolerance satisfied');
          break;
        case 1:
            console.log('limit of machine precision reached');
          break;
        case -2:
            console.log('\'parscale\' is too small relative to \'par\'');
          break;
        case 2: default:
            console.log('impossible error in subplex'); // # nocov
          break;
        }
    return [this.x0, this.fx]
}

module.exports = run;