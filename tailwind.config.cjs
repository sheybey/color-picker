/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ['./index.html', './src/Main.elm'],
  theme: {
    extend: {
      gridTemplateColumns: {
        ranges: 'max-content 1fr max-content',
      }
    },
  },
  plugins: [],
}

