<template>
  <div>
    <h3>Send a Signature Request to Only Others</h3>
    <div class="input-div-width">
      <label for="emails">Signer emails:</label>
      <input
        id="emails"
        v-model="signerEmails"
        class="input-width"
        type="text"
        placeholder="Enter comma-separated emails of signers in the desired signing order"
      />
    </div>
    <br />
    <SendSignatureRequest />
  </div>
</template>

<script>
import SendSignatureRequest from "./SendSignatureRequest";
import { mapMutations, mapGetters } from "vuex";

export default {
  name: "SignatureRequestOnlyOthers",
  components: {
    SendSignatureRequest
  },
  data() {
    return {
      signerEmails: [],
      timeoutHandle: () => {}
    };
  },
  computed: {
    ...mapGetters("documents", ["documentURL"])
  },
  watch: {
    signerEmails() {
      const vueComponent = this;
      this.setCanSendSignatureRequest(false);
      clearTimeout(this.timeoutHandle);

      this.timeoutHandle = setTimeout(function updateBody() {
        const signersArray = [];
        let curOrder = 0;
        const emails = vueComponent.signerEmails.split(",");
        for (const signerEmail of emails) {
          signersArray.push({ email: signerEmail, order: curOrder });
          curOrder++;
        }
        const requestBody = {
          document: vueComponent.documentURL,
          signers: signersArray,
          who: "o"
        };
        vueComponent.setRequestBody(requestBody);
        vueComponent.setCanSendSignatureRequest(true);
      }, 1500);
    }
  },
  created() {
    this.setCanSendSignatureRequest(false);
  },
  methods: {
    ...mapMutations("signatureRequests", [
      "setRequestBody",
      "setCanSendSignatureRequest"
    ])
  }
};
</script>
