/*
 * communicate with izuna's server
 */
export class IzunaServerService {

  constructor(serverUrl, pullRequestInfo) {
    this.serverUrl = serverUrl;
    this.user = pullRequestInfo.user;
    this.repo = pullRequestInfo.repo;
    this.pr = pullRequestInfo.pr;
  }

  getPrDetailsUrl() {
    return [ this.serverUrl, 'api', 'pullRequestInfo', this.user, this.repo, this.pr ].join('/');
  }

  getFileInfoUrl(commitId) {
    return [ this.serverUrl, 'api', 'projectInfo', this.user, this.repo, commitId ].join('/');
  }

  /*
   * from a given PR, find the target commit oid (i.e: the commit we are diffing from)
   * and the latest commit oid (i.e: the latest commit we pushed for this PR)
   */
  fetchPullRequestCommitsDetails() {
    return fetch(this.getPrDetailsUrl())
      .then(response => {
        if(response.ok) {
          return response.json();
        } else {
          throw `Could not fetch pull request information for this PR! Response status: ${response.status} for url: ${response.url}`;
        }
      });
  }

  fetchFilesInfo(pullRequestDetails, files) {
    const oldFilesInfo = this.fetchFileInfo(pullRequestDetails.targetOid, files);
    const newFilesInfo = this.fetchFileInfo(pullRequestDetails.commitOids[0], files);
    return Promise.all([oldFilesInfo, newFilesInfo]).then(filesInfo => {
      return {
        oldPackageInfo: filesInfo[0],
        newPackageInfo: filesInfo[1]
      };
    });
  }

  async fetchFileInfo(commitId, files) {
    return fetch( this.getFileInfoUrl(commitId),
      { method: 'POST',
        credentials: 'omit',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(files)
      }
    )
      .then(response => {
        if (response.ok) {
          return response.json();
        } else if (response.status === 404) {
          return {};
        } else {
          throw `Could not fetch izuna project info! Response status: ${response.status} for url: ${response.url}`;
        }
      });
  }

}
