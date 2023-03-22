export default class BrowserDetector {
  private userAgent: string

  private dataBrowser: Record<'subString' | 'identity', string>[]

  private versionSubString: string

  public browser: string

  public version: number | 'Unknown'

  constructor() {
    this.userAgent = navigator.userAgent

    this.dataBrowser = [
      { subString: 'Edge', identity: 'Edge' },
      { subString: 'MSIE', identity: 'Explorer' },
      { subString: 'Trident', identity: 'Explorer' },
      { subString: 'Firefox', identity: 'Firefox' },
      { subString: 'Opera', identity: 'Opera' },
      { subString: 'OPR', identity: 'Opera' },
      { subString: 'Chrome', identity: 'Chrome' },
      { subString: 'Safari', identity: 'Safari' },
    ]

    this.browser = this.searchString() || 'Other'
    this.version = this.searchVersion(navigator?.userAgent) || this.searchVersion(navigator?.appVersion) || 'Unknown'
  }

  private searchString(): string | null {
    const currentVersion = this.dataBrowser.find(({ subString }) => this.userAgent.indexOf(subString) !== -1)

    if (currentVersion) {
      this.versionSubString = currentVersion.subString
      return currentVersion.identity
    }

    return null
  }

  private searchVersion(dataString = ''): number | null {
    const index = dataString.indexOf(this.versionSubString)
    if (index === -1) {
      return null
    }

    const rv = dataString.indexOf('rv:')
    if (this.versionSubString === 'Trident' && rv !== -1) {
      return parseFloat(dataString.substring(rv + 3))
    }

    return parseFloat(dataString.substring(index + this.versionSubString.length + 1))
  }
}
