var subplex = {
     nsmin : 0,
     nsmax : 0,
     nfxe : 0,
     alpha : 0.0,
     beta : 0.0,
     gamma : 0.0,
     delta : 0.0,
     psi : 0.0,
     omega : 0.0,
     fxstat : Array(4),
     ftest : 0.0,
     initx : true,
     
     x0 : [],
     n : 0,
     f : {},
     work : [],
     iwork : [],
     fx : 0,
     nfe : 0,
     iflag : 0,

     tol : 2.220446e-16,
     maxnfe : 10000,
     scale : [1],
     hessian : false
}
require('./darray.js')

subplex.evalf = require('./evalf.js')
subplex.dcopy = require('./dcopy.js')
subplex.dscal = require('./dscal.js')
subplex.daxpy = require('./daxpy.js')
subplex.calcc = require('./calcc.js')
subplex.subopt = require('./subopt.js')
subplex.sortd = require('./sortd.js')
subplex.partx = require('./partx.js')
subplex.setstp = require('./setstp.js')
subplex.start = require('./start.js')
subplex.order = require('./order.js')
subplex.newpt = require('./newpt.js')
subplex.dasum = require('./dasum.js')
subplex.dist = require('./dist.js')
subplex.fstats = require('./fstats.js')
subplex.simplx = require('./simplx.js')
subplex.subplx = require('./subplx.js')
subplex.run = require('./run.js')

module.exports = subplex;