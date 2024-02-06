import {
  Button,
  FormControl,
  FormLabel,
  Input,
  SimpleGrid,
  Stack,
  Text,
  Textarea,
} from '@chakra-ui/react'
import { useUpdateAccount } from '@liteflow/react'
import useTranslation from 'next-translate/useTranslation'
import { FC, useEffect } from 'react'
import { useForm } from 'react-hook-form'
import { Account } from '../../../graphql'
import useSigner from '../../../hooks/useSigner'
import Dropzone from '../../Dropzone/Dropzone'

type FormData = {
  name: string
  description: string
  email: string
  twitter: string
  instagram: string
  website: string
  image: File | string
  cover: File | string
}

type Props = {
  account: Pick<
    Account,
    | 'cover'
    | 'image'
    | 'description'
    | 'email'
    | 'instagram'
    | 'name'
    | 'twitter'
    | 'website'
  >
  onUpdated: (address: string) => void
  onError: (error: unknown) => void
}

const UserFormEdit: FC<Props> = ({ account, onUpdated, onError }) => {
  const { t } = useTranslation('components')
  const signer = useSigner()
  const {
    control,
    register,
    handleSubmit,
    reset,
    formState: { isSubmitting },
  } = useForm<FormData>({
    defaultValues: {
      cover: account.cover || undefined,
      image: account.image || undefined,
      description: account.description || undefined,
      email: account.email || undefined,
      instagram: account.instagram || undefined,
      name: account.name || undefined,
      twitter: account.twitter || undefined,
      website: account.website || undefined,
    },
  })

  useEffect(() => {
    reset({
      cover: account.cover || undefined,
      image: account.image || undefined,
      description: account.description || undefined,
      email: account.email || undefined,
      instagram: account.instagram || undefined,
      name: account.name || undefined,
      twitter: account.twitter || undefined,
      website: account.website || undefined,
    })
  }, [account, reset])

  const [editAccount] = useUpdateAccount(signer)

  const onSubmit = handleSubmit(async (data) => {
    try {
      const address = await editAccount(data)
      onUpdated(address)
    } catch (error) {
      onError(error)
    }
  })

  return (
    <SimpleGrid
      as="form"
      mt={12}
      rowGap={{ base: 8, md: 0 }}
      gap={{ base: 0, md: 8, lg: 12 }}
      templateColumns={{ md: '264px 1fr' }}
      onSubmit={onSubmit}
    >
      <Dropzone
        label={t('user.form.edit.image.label')}
        heading={t('user.form.edit.image.heading')}
        hint={t('user.form.edit.image.hint')}
        acceptTypes={{ 'image/*': ['.jpg', '.jpeg', '.png', '.gif', '.webp'] }}
        maxSize={10000000} // 10 MB
        name="image"
        control={control}
        rounded
        withPlaceholder
        value={account.image || undefined}
        context={{
          replace: t('user.form.edit.image.file.replace'),
          chose: t('user.form.edit.image.file.chose'),
        }}
      />
      <Stack spacing={8}>
        <Dropzone
          label={t('user.form.edit.cover.label')}
          heading={t('user.form.edit.cover.heading')}
          hint={t('user.form.edit.cover.hint')}
          acceptTypes={{
            'image/*': ['.jpg', '.jpeg', '.png', '.gif', '.webp'],
          }}
          maxSize={10000000} // 10 MB
          name="cover"
          control={control}
          withPlaceholder
          value={account.cover || undefined}
          context={{
            replace: t('user.form.edit.cover.file.replace'),
            chose: t('user.form.edit.cover.file.chose'),
          }}
        />
        <FormControl>
          <FormLabel htmlFor="name">{t('user.form.edit.name.label')}</FormLabel>
          <Input
            id="name"
            placeholder={t('user.form.edit.name.placeholder')}
            {...register('name')}
          />
        </FormControl>
        <FormControl>
          <FormLabel htmlFor="description">
            {t('user.form.edit.description.label')}
          </FormLabel>
          <Textarea
            id="description"
            placeholder={t('user.form.edit.description.placeholder')}
            {...register('description')}
            rows={5}
          />
        </FormControl>
        <FormControl>
          <FormLabel htmlFor="email">
            {t('user.form.edit.email.label')}
          </FormLabel>
          <Input
            id="email"
            type="email"
            placeholder={t('user.form.edit.email.placeholder')}
            {...register('email', {
              pattern: {
                value: /^[\w%+.-]+@[\d.a-z-]+\.[a-z]{2,}$/i,
                message: t('user.form.edit.email.validation.invalid'),
              },
            })}
          />
        </FormControl>
        <FormControl>
          <FormLabel htmlFor="twitter">
            {t('user.form.edit.twitter.label')}
          </FormLabel>
          <Input
            id="twitter"
            placeholder={t('user.form.edit.twitter.placeholder')}
            {...register('twitter')}
          />
        </FormControl>
        <FormControl>
          <FormLabel htmlFor="instagram">
            {t('user.form.edit.instagram.label')}
          </FormLabel>
          <Input
            id="instagram"
            placeholder={t('user.form.edit.instagram.placeholder')}
            {...register('instagram')}
          />
        </FormControl>
        <FormControl>
          <FormLabel htmlFor="website">
            {t('user.form.edit.website.label')}
          </FormLabel>
          <Input
            id="website"
            type="url"
            placeholder={t('user.form.edit.website.placeholder')}
            {...register('website')}
          />
        </FormControl>
        <Button size="lg" isLoading={isSubmitting} type="submit" width="full">
          <Text as="span" isTruncated>
            {t('user.form.edit.submit')}
          </Text>
        </Button>
      </Stack>
    </SimpleGrid>
  )
}

export default UserFormEdit
