<template>
  <div>
    <button :disabled="!canSendSignatureRequest" @click="sendSignatureRequest">
      Send Signature Request
    </button>
    <p v-if="attempted && sendSucceeded" class="success">
      <strong
        >&#10003; Signature request sent successfully: ID
        {{ signatureRequestUUID }}</strong
      >
    </p>
    <p v-if="attempted && !sendSucceeded" class="error">
      <strong>Sending failed: {{ sendErrorDescription }}</strong>
    </p>
    <a v-if="sendSucceeded" :href="signingUrl" target="_blank"
      >Click on the link to sign</a
    >
  </div>
</template>

<script>
import { mapState, mapGetters } from "vuex";

export default {
  name: "SendSignatureRequest",
  data() {
    return {
      attempted: false,
      sendSucceeded: false,
      sendErrorDescription: "",
      signingUrl: ""
    };
  },
  computed: {
    ...mapState("signatureRequests", [
      "requestBody",
      "canSendSignatureRequest"
    ]),
    ...mapState("signatureRequests", ["signatureRequestUUID"])
  },
  methods: {
    sendSignatureRequest() {
      this.$store
        .dispatch("signatureRequests/sendSignatureRequest", {
          body: this.requestBody
        })
        .then(result => {
          this.signingUrl = result.body.signers[0].embed_url;
          this.attempted = true;
          this.sendSucceeded = true;
        })
        .catch(error => {
          this.attempted = true;
          this.sendErrorDescription = error;
          this.sendSucceeded = false;
        });
    }
  }
};
</script>
