const gulp = require('gulp');
const exec = require('child_process').exec;
const uglify = require('gulp-uglify');
const pump = require('pump');
const zip = require('gulp-zip');
const runSequence = require('run-sequence');

gulp.task('browserify', (cb) => {
  exec('pulp browserify --optimise --to extension/clearnexus.js', (err, stdout, stderr) => {
    console.log(stdout);
    console.log(stderr);
    cb(err);
  });
});

gulp.task('compress', () => {
    pump([
        gulp.src('extension/clearnexus.js'),
        uglify(),
        gulp.dest('extension')
    ] 
  );
});

gulp.task('package', () =>
    gulp.src('extension/*')
        .pipe(zip('clearnexus.zip'))
        .pipe(gulp.dest('dist'))
);

gulp.task('build', () => {
    runSequence('browserify', 'compress', 'package');
});
