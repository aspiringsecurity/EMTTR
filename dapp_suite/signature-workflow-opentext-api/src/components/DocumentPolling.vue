<template>
  <div>
    <h3>Final Document</h3>
    <p v-if="!polling && !documentRetrieved">
      Signature request has not been sent yet.
    </p>
    <p v-if="polling">{{ pollingMessage }}</p>
    <p v-if="polling && pollErrorDescription" class="error">
      Polling error: {{ pollErrorDescription }}
    </p>
    <div v-if="!deleteSucceeded">
      <div class="input-div-width">
        <div class="input-width">
          <a v-if="documentRetrieved" :href="document.pdf" target="_blank"
            >Download final document</a
          >
        </div>
        <div class="input-width">
          <a
            v-if="documentRetrieved"
            :href="document.signing_log.pdf"
            target="_blank"
            >Download signing log</a
          >
        </div>
      </div>
      <br />
      <button v-if="documentRetrieved" @click="deleteDocument">
        Delete the Document
      </button>
      <p v-if="attempted && !deleteSucceeded" class="error">
        <strong>Deletion failed: {{ deleteErrorDescription }}</strong>
      </p>
    </div>
    <p v-if="attempted && deleteSucceeded" class="success">
      <strong>&#10003; Deletion succeeded!</strong>
    </p>
  </div>
</template>

<script>
import { mapState, mapGetters } from "vuex";

export default {
  name: "DocumentPolling",
  data() {
    return {
      attempted: false,
      deleteSucceeded: false,
      deleteErrorDescription: "",
      document: {},
      documentRetrieved: false,
      interval: undefined,
      pollErrorDescription: "",
      polling: false,
      pollingMessage: "Polling for final document"
    };
  },
  computed: {
    ...mapGetters("auth", ["authHeader"]),
    ...mapGetters("documents", ["documentURL", "documentUUID"]),
    ...mapGetters("systemInfo", ["systemInfo"]),
    ...mapState("events", ["eventsEndpoint"]),
    ...mapState("signatureRequests", [
      "signatureRequestUUID",
      "signatureRequestsEndpoint"
    ]),
    deleteDocumentURL() {
      return `${this.systemInfo.apiHost}${this.documentURL}`;
    },
    eventsURLQuerySigned() {
      return `${this.systemInfo.apiHost}${this.eventsEndpoint}?document__uuid=${this.documentUUID}&event_type=signed`;
    }
  },
  watch: {
    signatureRequestUUID: {
      immediate: true,
      handler(newVal) {
        if (newVal) {
          this.interval = setInterval(
            function doInterval() {
              if (this.signatureRequestUUID) {
                this.polling = true;
                this.pollingMessage += ".";
                this.pollSignedDocument();
              }
            }.bind(this),
            3000
          );
        }
      }
    },
  },
  methods: {
    deleteDocument() {
      this.$http
        .delete(this.deleteDocumentURL, {
          headers: this.authHeader
        })
        .then(result => {
          this.attempted = true;
          this.deleteSucceeded = true;
        })
        .catch(error => {
          this.attempted = true;
          this.deleteErrorDescription = error;
          this.deleteSucceeded = false;
          console.error(error);
        });
    },
    pollSignedDocument() {
      this.$http
        .get(this.eventsURLQuerySigned, {
          headers: this.authHeader
        })
        .then(result => {
          if (result.body.count === 1) {
            this.document = result.body.results[0].document;
            this.documentRetrieved = true;
            this.polling = false;
            clearInterval(this.interval);
          }
        })
        .catch(error => {
          this.pollErrorDescription = error;
          console.error(error);
        });
    }
  }
};
</script>
