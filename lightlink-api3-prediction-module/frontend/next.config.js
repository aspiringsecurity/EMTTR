/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: false,
  env: {
    WALLET_CLOUD_PROJECT_ID: "b18a057500f1ca54b904df37574c961b",
  },
  eslint: {
    ignoreDuringBuilds: true,
  },
};

module.exports = nextConfig;
