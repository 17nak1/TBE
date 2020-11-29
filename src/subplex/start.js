let start = function(n,x,step,ns,ips,s,small){
    let i,j
    for(i = 1; i<= ns; i++){
        s[i][1] = x[ips[i]]
    }

    for(j = 2; j<= ns+1; j++){
        this.dcopy (ns,s.clone(),1,s.clone((j-1)*ns+1),1)
        s[j-1][j] = s[j-1][1]+step[ips[j-1]]
    }

    for(j = 2; j<= ns+1; j++){
        if (s[j-1][j] === s[j-1][1]){
            small[0]  = true
            return
        }
    }
    small[0] = false;
}

module.exports = start;