module.exports = {
   purge: {
      enabled: true,
      content: ["./src/**/*.elm"],
     defaultExtractor: (content) => { mmatchado = content.match(/class\s+"(.+)"/).groups || [];
                                      return mmatchado
                                    }
   },
   darkMode: false, // or 'media' or 'class'
   theme: {
      extend: {},
   },
   variants: {},
   plugins: [
      require("@tailwindcss/typography"),
      require("@tailwindcss/forms"),
      require("@tailwindcss/line-clamp"),
      require("@tailwindcss/aspect-ratio"),
   ],
};
