/** @type {import('next').NextConfig} */
const nextConfig = {
    reactStrictMode: true,
    swcMinify: true,
    env: {
        WEB3STORAGE_TOKEN: process.env.WEB3STORAGE_TOKEN,
    },
}

module.exports = nextConfig
