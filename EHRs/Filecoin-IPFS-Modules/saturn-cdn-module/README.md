# saturn CDN EMTTR Module

## Requirements

- npx

## Browser client integration

1. Add this script tag to the <head> tag. `<script src="https://saturn.tech/widget.js" async></script>`. This will install the service worker.
2. Fetch the service worker.
`curl -o saturn-sw.js https://saturn.tech/saturn-sw.js`
3. Add the service worker JS file to the root path of your domain.
4. Run `npx serve`
5. Navigate to `http://localhost:3000`


