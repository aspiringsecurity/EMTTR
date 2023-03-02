const axios = require('axios')

axios.get('http://192.168.10.130:3000/password/hash?password=5')
.then(res => console.log(res.data));
