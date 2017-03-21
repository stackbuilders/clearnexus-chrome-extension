var fs = require('fs');
var env = process.env.PURS_ENV || 'Prod';
var path_origin = './support/' + env + '.purs';
var path_dest = './src/Config.purs';

// Copy config module
fs.createReadStream(path_origin).pipe(fs.createWriteStream(path_dest));
