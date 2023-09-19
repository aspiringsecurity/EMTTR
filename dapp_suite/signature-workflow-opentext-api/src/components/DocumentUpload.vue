<template>
  <div>
    <h3>Upload a Document</h3>
    <span class="input-div-width">
      <label class="inline-label" for="autoDelete"
        >Number of days before the finished document is deleted:</label
      >
      <input
        id="autoDelete"
        v-model="autoDeleteDays"
        type="number"
        placeholder="Enter the number of days before the finished document is deleted"
      />
    </span>
    <br /><br />
    <span class="input-div-width">
      <label class="inline-label" for="publicLink"
        >Number of days the documents are public after signing
        completion:</label
      >
      <input
        id="publicLink"
        v-model="publicLinkDays"
        type="number"
        placeholder="Enter the number of days the finished documents are public to anonymous signers"
      />
    </span>
    <br /><br />
    <label for="fileSelection">Select a file to upload</label>
    <div class="input-div-width">
      <input id="fileSelection" type="file" @change="processFile($event)" />
    </div>
    <br />
    <button @click="uploadDocument">
      Upload Document to Core Signature
    </button>
    <p v-if="attempted && uploadSucceeded" class="success">
      <strong
        >&#10003; Document uploaded successfully: ID {{ documentUUID }}</strong
      >
    </p>
    <p v-if="attempted && !uploadSucceeded" class="error">
      <strong
        >Upload to Core Signature failed: {{ uploadErrorDescription }}</strong
      >
    </p>
  </div>
</template>

<script>
import { mapState, mapGetters } from "vuex";

export default {
  name: "DocumentUpload",
  data() {
    return {
      attempted: false,
      autoDeleteDays: 1,
      publicLinkDays: 0,
      file: undefined,
      fileBase64Content: "",
      uploadSucceeded: false,
      uploadErrorDescription: ""
    };
  },
  computed: {
    ...mapGetters("systemInfo", ["systemInfo"]),
    ...mapState("documents", ["documentsEndpoint", "documentUUID"]),
    authHeader() {
      return {
        Authorization: this.$store.getters["auth/user"].accessToken,
        "Content-Type": "application/json"
      };
    },
    uploadBody() {
      const body = {
        file_from_content: this.fileBase64Content,
        file_from_content_name: this.file.name,
        auto_delete_days: this.autoDeleteDays
      };

      if (this.publicLinkDays > 0) {
        body.link_expire_days = this.publicLinkDays;
      }
      return body;
    },
    uploadURL() {
      return `${this.systemInfo.apiHost}${this.documentsEndpoint}`;
    }
  },
  methods: {
    async processFile(event) {
      this.file = event.target.files[0];
      this.fileBase64Content = await this.toBase64(this.file);
    },
    toBase64(file) {
      return new Promise((resolve, reject) => {
        const reader = new FileReader();
        reader.readAsDataURL(file);
        reader.onload = () => resolve(reader.result.split(",")[1]);
        reader.onerror = error => reject(error);
      });
    },
    uploadDocument() {
      this.$http
        .post(this.uploadURL, this.uploadBody, {
          headers: this.authHeader
        })
        .then(result => {
          this.attempted = true;
          this.uploadSucceeded = true;
          this.$store.commit("documents/setDocumentUUID", result.body.uuid);
        })
        .catch(error => {
          this.attempted = true;
          this.uploadErrorDescription = error;
          this.uploadSucceeded = false;
          console.error(error);
        });
    }
  }
};
</script>

<style scoped>
.inline-label {
  margin-right: 10px;
  font-weight: normal;
}
</style>
