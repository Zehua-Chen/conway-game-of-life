import gulp from "gulp";
import path from "path";
import gulpZip from "gulp-zip";
import fs from "fs";

export function clean(cb) {
  if (fs.existsSync("dist")) {
    fs.rm("dist", { recursive: true }, cb);
  } else {
    cb();
  }
}

export function distReport() {
  return gulp
    .src(path.join("report", "bin", "report.pdf"))
    .pipe(gulp.dest("dist"));
}

export function distProposal() {
  return gulp
    .src(path.join("proposal", "bin", "proposal.pdf"))
    .pipe(gulp.dest("dist"));
}

export function distSlides() {
  return gulp.src(path.join("slides.pdf")).pipe(gulp.dest("dist"));
}

export function distHaskell() {
  return gulp
    .src(
      [
        path.join("app", "**"),
        path.join("src", "**"),
        path.join("test", "**"),
        path.join(".gitignore"),
        path.join("ChangeLog.md"),
        path.join("conway.cabal"),
        path.join("package.yaml"),
        path.join("README.md"),
        path.join("References.md"),
        path.join("Setup.hs"),
        path.join("stack.yaml"),
        path.join("stack.yaml.lock"),
      ],
      { base: "." }
    )
    .pipe(gulpZip("source.zip"))
    .pipe(gulp.dest("dist"));
}

export const dist = gulp.series(
  clean,
  gulp.parallel(distReport, distProposal, distSlides, distHaskell)
);
