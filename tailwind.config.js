module.exports = {
   purge: {
      enabled: true,
      content: ["./src/**/*.elm"],
      defaultExtractor: (content) => {
         mmatchado = content.match(/class\s+"(.+)"/).groups || [];
         return mmatchado;
      },
   },
   darkMode: false, // or 'media' or 'class'
   theme: {
      extend: {
         fontFamily: {
            sans: [
               "Fira Sans Condensed",
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
           serif: ["literata", "Georgia", "Cambria", '"Times New Roman"', "Times", "serif"],
         },
      },
   },
   variants: {},
   plugins: [
      require("@tailwindcss/typography"),
      require("@tailwindcss/forms"),
      require("@tailwindcss/line-clamp"),
      require("@tailwindcss/aspect-ratio"),
   ],
};
