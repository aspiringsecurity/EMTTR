import { Box, Divider, Flex, SimpleGrid, Stack } from '@chakra-ui/react'
import useTranslation from 'next-translate/useTranslation'
import { ReactElement } from 'react'
import useEnvironment from '../../hooks/useEnvironment'
import Empty from '../Empty/Empty'
import type { IProp as PaginationProps } from '../Pagination/Pagination'
import Pagination from '../Pagination/Pagination'
import Select from '../Select/Select'
import SkeletonGrid from '../Skeleton/Grid'
import SkeletonTokenCard from '../Skeleton/TokenCard'
import type { Props as NFTCardProps } from './Card'
import NFTCard from './Card'

type IProps<Order extends string> = {
  assets: NFTCardProps['asset'][] | undefined
  orderBy: {
    value: Order
    choices: {
      value: Order
      label: string
    }[]
    onSort: (orderBy: any) => Promise<void>
  }
  pagination: PaginationProps
}

const TokenGrid = <Order extends string>({
  assets,
  orderBy,
  pagination,
}: IProps<Order>): ReactElement => {
  const { PAGINATION_LIMIT } = useEnvironment()
  const { t } = useTranslation('components')
  return (
    <Stack spacing={6}>
      <Box ml="auto" w={{ base: 'full', md: 'min-content' }}>
        <Select
          label={t('token.grid.sort.label')}
          name="orderBy"
          onChange={orderBy.onSort}
          choices={orderBy.choices}
          value={orderBy.value}
          inlineLabel
        />
      </Box>
      {assets === undefined ? (
        <SkeletonGrid
          items={
            pagination.withoutLimit
              ? PAGINATION_LIMIT
              : pagination.limit || PAGINATION_LIMIT
          }
          compact
          spacing={{ base: 4, lg: 3, xl: 4 }}
          columns={{ base: 1, sm: 2, md: 3 }}
        >
          <SkeletonTokenCard />
        </SkeletonGrid>
      ) : assets.length > 0 ? (
        <SimpleGrid
          flexWrap="wrap"
          spacing={{ base: 4, lg: 3, xl: 4 }}
          columns={{ base: 1, sm: 2, md: 3 }}
        >
          {assets.map((asset, i) => (
            <Flex key={i} justify="center">
              <NFTCard asset={asset} />
            </Flex>
          ))}
        </SimpleGrid>
      ) : (
        <Empty
          title={t('token.grid.empty.title')}
          description={t('token.grid.empty.description')}
          button={t('token.grid.empty.action')}
          href="/explore"
        />
      )}
      <Divider display={assets?.length !== 0 ? 'block' : 'none'} />
      {assets?.length !== 0 && <Pagination {...pagination} />}
    </Stack>
  )
}

export default TokenGrid
