import axios from 'axios';

class Tasks {
  constructor(user, taskName) {
    this.url = `${process.env.REACT_APP_BASE_SERVICE_URL}/workflow/v1/tasks`;
    this.user = user;
    this.taskName = taskName;
  }

  async getTasks(offset) {
    return axios({
      method: 'get',
      url: `${this.url}?sort=createTime&order=desc&name=${encodeURIComponent(this.taskName)}&candidateOrAssigned=${encodeURIComponent(this.user.profile.preferred_username)}&includeProcessVariables=true${(offset ? `&offset=${offset}` : '')}`,
      headers: {
        Authorization: `Bearer ${this.user.access_token}`,
      },
    }).catch((error) => {
      // eslint-disable-next-line no-alert
      alert(
        error.response != null && error.response.data != null ? error.response.data : error.message,
      );
    });
  }

  async claimTask(taskId) {
    return axios({
      method: 'post',
      url: `${this.url}/${taskId}`,
      headers: {
        Authorization: `Bearer ${this.user.access_token}`,
      },
      data: {
        action: 'claim',
        assignee: this.user.profile.preferred_username,
      },
    }).catch((error) => {
      // eslint-disable-next-line no-alert
      alert(
        error.response != null && error.response.data != null ? error.response.data : error.message,
      );
    });
  }

  async completeTask(taskId, approved) {
    return axios({
      method: 'post',
      url: `${this.url}/${taskId}`,
      headers: {
        Authorization: `Bearer ${this.user.access_token}`,
      },
      data: {
        action: 'complete',
        outcome: approved ? 'approved' : 'rejected',
        variables: [
          {
            name: 'approver',
            value: this.user.profile.preferred_username,
          },
        ],
      },
    });
  }
}

export default Tasks;
