import { Button } from '@eth-optimism/ui-components/src/components/ui/button'
import { Card } from '@eth-optimism/ui-components/src/components/ui/card'
import { Text } from '@eth-optimism/ui-components/src/components/ui/text'
import { RiCompasses2Line } from '@remixicon/react'

export default function Insights() {
  return (
    <main className="flex justify-center relative">
      <Banner />
      <Card className="max-w-7xl w-full mt-36 mx-8 mb-16 z-10 p-16 flex flex-col items-center">
        <RiCompasses2Line size={64} className="mb-4" />
        <Text as="h2" className="text-base mb-1 font-semibold">
          We're still working on insights
        </Text>
        <Text as="p" className="text-muted-foreground text-center mb-4">
          What kind of content would be valuable to you?
        </Text>
        <Button variant="secondary" asChild>
          <a href="">
            <Text as="span">Let us know</Text>
          </a>
        </Button>
      </Card>
    </main>
  )
}

const Banner = () => {
  return <div className="absolute -inset-x-0 w-full h-80 bg-red-200" />
}
