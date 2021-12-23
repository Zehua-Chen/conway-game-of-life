import gulp from "gulp";
import path from "path";
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
    .pipe(gulp.dest("dist"));
}

export const zip = gulp.series(
  clean,
  gulp.parallel(distReport, distSlides, distHaskell)
);
