<template>
  <div>
    <h3>Send a Signature Request to Only Me</h3>
    <SendSignatureRequest />
  </div>
</template>

<script>
import SendSignatureRequest from "./SendSignatureRequest";
import { mapMutations, mapGetters } from "vuex";

export default {
  name: "SignatureRequestOnlyMe",
  components: {
    SendSignatureRequest
  },
  computed: {
    ...mapGetters("auth", ["authHeader", "user"]),
    ...mapGetters("documents", ["documentURL"])
  },
  created() {
    const signers = [];
    signers.push({ email: this.user.username, embed_url_user_id: "1234" });

    const requestBody = {
      document: this.documentURL,
      signers: signers,
      who: "m"
    };

    this.setRequestBody(requestBody);
    this.setCanSendSignatureRequest(true);
  },
  methods: {
    ...mapMutations("signatureRequests", [
      "setRequestBody",
      "setCanSendSignatureRequest"
    ])
  }
};
</script>
