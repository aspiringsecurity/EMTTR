import type { Metadata } from 'next'
import { Inter } from 'next/font/google'
import '@/app/globals.css'
import { PrivyProviderWrapper } from '@/app/providers/PrivyProviderWrapper'
import { Header } from '@/app/components/Header'
import { ThemeProvider } from '@/app/providers/ThemeProvider'

const inter = Inter({ subsets: ['latin'] })

export const metadata: Metadata = {
  title: 'Supercain Developer Console',
  description: 'Lorem ipsum dolor sit amet, consectetur adipiscing elit.',
}

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode
}>) {
  return (
    <html lang="en">
      <body className={inter.className}>
        <ThemeProvider attribute="class" defaultTheme="system">
          <PrivyProviderWrapper>
            <Header />
            {children}
          </PrivyProviderWrapper>
        </ThemeProvider>
      </body>
    </html>
  )
}
