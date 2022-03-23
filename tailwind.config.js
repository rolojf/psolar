module.exports = {
   content: {
      files: ["./src/**/*.elm"],
      extract: {
         elm: (contenido) => {
            let matchado = contenido.match(/class\s+"(.+?)"/);
            let regresa = matchado ? matchado[1].split(" ") : [];
            //if (regresa.length > 0 ) {console.log(regresa)};
            return regresa;
         },
      },
   },
   theme: {
      extend: {
         fontFamily: {
            sans: [
               "Fira Sans",
               "system-ui",
               "-apple-system",
               "BlinkMacSystemFont",
               '"Segoe UI"',
               "Roboto",
               '"Helvetica Neue"',
               "Arial",
               '"Noto Sans"',
               "sans-serif",
               '"Apple Color Emoji"',
               '"Segoe UI Emoji"',
               '"Segoe UI Symbol"',
               '"Noto Color Emoji"',
            ],
            serif: ["Roboto Slab", "Georgia", "Cambria", '"Times New Roman"', "Times", "serif"],
         },
      },
   },
   plugins: [
      require("@tailwindcss/typography"),
      require("@tailwindcss/forms"),
      require("@tailwindcss/line-clamp"),
      require("@tailwindcss/aspect-ratio"),
   ],
};
