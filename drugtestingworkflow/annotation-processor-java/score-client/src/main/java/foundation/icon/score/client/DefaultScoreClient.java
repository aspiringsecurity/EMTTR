/*
 * Copyright 2021 ICON Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package foundation.icon.score.client;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import foundation.icon.jsonrpc.Address;
import foundation.icon.jsonrpc.IconJsonModule;
import foundation.icon.jsonrpc.JsonrpcClient;
import foundation.icon.jsonrpc.SendTransactionParamSerializer;
import foundation.icon.jsonrpc.TypeReference;
import foundation.icon.jsonrpc.model.CallData;
import foundation.icon.jsonrpc.model.CallParam;
import foundation.icon.jsonrpc.model.DeployData;
import foundation.icon.jsonrpc.model.Hash;
import foundation.icon.jsonrpc.model.SendTransactionParam;
import foundation.icon.jsonrpc.model.TransactionParam;
import foundation.icon.jsonrpc.model.TransactionResult;
import score.UserRevertedException;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class DefaultScoreClient extends JsonrpcClient {
    public static final Address ZERO_ADDRESS = new Address("cx0000000000000000000000000000000000000000");
    public static final BigInteger DEFAULT_STEP_LIMIT = new BigInteger("9502f900",16);
    public static final long DEFAULT_RESULT_RETRY_WAIT = 1000;
    public static final long DEFAULT_RESULT_TIMEOUT = 10000;

    protected final BigInteger nid;
    protected final Wallet wallet;
    protected final Address address;
    protected BigInteger stepLimit;
    protected long resultTimeout = DEFAULT_RESULT_TIMEOUT;
    protected long resultRetryWait = DEFAULT_RESULT_RETRY_WAIT;

    public DefaultScoreClient(String url, String nid, String keyStorePath, String keyStorePassword, String address) {
        this(url, nid(nid), wallet(keyStorePath, keyStorePassword), new Address(address));
    }

    public DefaultScoreClient(String url, BigInteger nid, Wallet wallet, Address address) {
        this(url, nid, DEFAULT_STEP_LIMIT, wallet, address);
    }

    public DefaultScoreClient(String url, BigInteger nid, BigInteger stepLimit, Wallet wallet, Address address) {
        super(url);
        initialize(this);

        this.nid = nid;
        this.stepLimit = stepLimit;
        this.wallet = wallet;
        this.address = address;
    }

    public DefaultScoreClient(DefaultScoreClient client) {
        this(client, client._wallet());
    }
    public DefaultScoreClient(DefaultScoreClient client, Wallet wallet) {
        super(client.endpoint);
        initialize(this);

        this.nid = client._nid();
        this.wallet = wallet;
        this.address = client._address();
        this.stepLimit = client._stepLimit();
        this.resultTimeout = client._resultTimeout();
        this.resultRetryWait = client._resultRetryWait();
    }

    static void initialize(JsonrpcClient client) {
        client.mapper().registerModule(new IconJsonModule());
        client.mapper().setSerializationInclusion(JsonInclude.Include.NON_NULL);
    }

    public static DefaultScoreClient _deploy(String url, String nid, String keyStorePath, String keyStorePassword, String scoreFilePath, Map<String, Object> params) {
        return _deploy(url, nid(nid), wallet(keyStorePath, keyStorePassword), scoreFilePath, params);
    }

    public static DefaultScoreClient _deploy(String url, BigInteger nid, Wallet wallet, String scoreFilePath, Map<String, Object> params) {
        return _deploy(url, nid, DEFAULT_STEP_LIMIT, wallet, scoreFilePath, params);
    }

    public static DefaultScoreClient _deploy(String url, BigInteger nid, BigInteger stepLimit, Wallet wallet, String scoreFilePath, Map<String, Object> params) {
        JsonrpcClient client = new JsonrpcClient(url);
        initialize(client);
        Address address = deploy(client, nid, wallet, stepLimit, ZERO_ADDRESS, scoreFilePath, params, DEFAULT_RESULT_TIMEOUT);
        return new DefaultScoreClient(url, nid, stepLimit, wallet, address);
    }

    public void _update(String scoreFilePath, Map<String, Object> params) {
        deploy(this, nid, wallet, DEFAULT_STEP_LIMIT, address, scoreFilePath, params, DEFAULT_RESULT_TIMEOUT);
    }

    public BigInteger _nid() {
        return nid;
    }

    public Wallet _wallet() {
        return wallet;
    }

    public Address _address() {
        return address;
    }

    public BigInteger _stepLimit() {
        return stepLimit;
    }

    public void _stepLimit(BigInteger stepLimit) {
        this.stepLimit = stepLimit;
    }

    public long _resultTimeout() {
        return resultTimeout;
    }

    public void _resultTimeout(long resultTimeout) {
        this.resultTimeout = resultTimeout;
    }

    public long _resultRetryWait() {
        return resultRetryWait;
    }

    public void _resultRetryWait(long resultRetryWait) {
        this.resultRetryWait = resultRetryWait;
    }

    public <T> T _call(Class<T> responseType, String method, Map<String, Object> params) {
        return call(this, responseType, address, method, params);
    }

    public <T> T _call(TypeReference<T> responseType, String method, Map<String, Object> params) {
        return call(this, responseType, address, method, params);
    }

    public TransactionResult _send(String method, Map<String, Object> params) {
        return send(this, nid, wallet, stepLimit, address, null, method, params, resultTimeout);
    }

    public TransactionResult _send(BigInteger valueForPayable, String method, Map<String, Object> params) {
        return send(this, nid, wallet, stepLimit, address, valueForPayable, method, params, resultTimeout);
    }

    public TransactionResult _transfer(Address to, BigInteger value, String message) {
        return transfer(this, nid, wallet, stepLimit, to, value, message, resultTimeout);
    }

    public BigInteger _balance() {
        return _balance(address);
    }

    public BigInteger _balance(Address address) {
        return balance(this, address);
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    static class BlockHeight {
        BigInteger height;

        public BigInteger getHeight() {
            return height;
        }
    }

    public BigInteger _lastBlockHeight() {
        return lastBlock(this, BlockHeight.class).height;
    }


    public static DefaultScoreClient of(Properties properties) {
        return of("", properties);
    }

    public static DefaultScoreClient of(Properties properties, Map<String, Object> params) {
        return of("", properties, params);
    }

    public static DefaultScoreClient of(String prefix, Properties properties) {
        return of(prefix, properties, null);
    }

    public static DefaultScoreClient of(String prefix, Properties properties, Map<String, Object> params) {
        String url = url(prefix, properties);
        BigInteger nid = nid(prefix, properties);
        BigInteger stepLimit = stepLimit(prefix, properties);
        if (stepLimit == null) {
            stepLimit = DEFAULT_STEP_LIMIT;
        }
        Wallet wallet = wallet(prefix, properties);
        Address address = address(prefix, properties);
        String scoreFilePath = scoreFilePath(prefix, properties);
        Map<String, Object> deployParams = params(prefix, properties, params);
        if (address == null) {
            System.out.printf("deploy prefix: %s, url: %s, nid: %s, stepLimit: %s, keyStorePath: %s, scoreFilePath: %s, params: %s%n",
                    prefix, url, nid, stepLimit, wallet != null ? wallet.getAddress() : wallet, scoreFilePath, deployParams);
            return _deploy(url, nid, stepLimit, wallet, scoreFilePath, deployParams);
        } else {
            System.out.printf("prefix: %s, url: %s, nid: %s, stepLimit: %s, wallet: %s, address: %s%n",
                    prefix, url, nid, stepLimit, wallet != null ? wallet.getAddress() : wallet, address);
            DefaultScoreClient client = new DefaultScoreClient(url, nid, stepLimit, wallet, address);
            if (isUpdate(prefix, properties) && scoreFilePath != null && !scoreFilePath.isEmpty()) {
                System.out.printf("update scoreFilePath: %s, params: %s%n", scoreFilePath, deployParams);
                client._update(scoreFilePath, deployParams);
            }
            return client;
        }
    }

    public static String url(Properties properties) {
        return url("", properties);
    }

    public static String url(String prefix, Properties properties) {
        return properties.getProperty(prefix+"url");
    }

    public static BigInteger nid(Properties properties) {
        return nid("", properties);
    }

    public static BigInteger nid(String prefix, Properties properties) {
        return nid(properties.getProperty(prefix+"nid"));
    }

    public static BigInteger nid(String nid) {
        if (nid == null) {
            return null;
        } else if (nid.startsWith("0x")) {
            return new BigInteger(nid.substring(2), 16);
        } else {
            return new BigInteger(nid);
        }
    }

    public static BigInteger stepLimit(Properties properties) {
        return stepLimit("", properties);
    }

    public static BigInteger stepLimit(String prefix, Properties properties) {
        return stepLimit(properties.getProperty(prefix+"stepLimit"));
    }

    public static BigInteger stepLimit(String stepLimit) {
        if (stepLimit == null) {
            return null;
        } else if (stepLimit.startsWith("0x")) {
            return new BigInteger(stepLimit.substring(2), 16);
        } else {
            return new BigInteger(stepLimit);
        }
    }

    public static Wallet wallet(Properties properties) {
        return wallet("", properties);
    }

    public static Wallet wallet(String prefix, Properties properties) {
        String keyStore = properties.getProperty(prefix+"keyStore");
        if (keyStore == null || keyStore.isEmpty()) {
            return null;
        }
        String keyPassword = properties.getProperty(prefix+"keyPassword");
        if (keyPassword == null || keyPassword.isEmpty()) {
            String keySecret = properties.getProperty(prefix+"keySecret");
            try {
                System.out.println("using keySecret "+keySecret);
                keyPassword = Files.readString(Path.of(keySecret));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        return wallet(keyStore, keyPassword);
    }

    public static Wallet wallet(String keyStorePath, String keyStorePassword) {
        System.out.println("load wallet "+keyStorePath);
        return Wallet.load(keyStorePassword, new File(keyStorePath));
    }

    public static Address address(Properties properties) {
        return address("", properties);
    }

    public static Address address(String prefix, Properties properties) {
        String address = properties.getProperty(prefix+"address");
        if (address == null || address.isEmpty()) {
            return null;
        }
        return address(address);
    }

    public static Address address(String address) {
        return new Address(address);
    }

    public static boolean isUpdate(Properties properties) {
        return isUpdate("", properties);
    }

    public static boolean isUpdate(String prefix, Properties properties) {
        return Boolean.parseBoolean(
                (String)properties.getOrDefault(prefix+"isUpdate",
                        Boolean.FALSE.toString()));
    }

    public static String scoreFilePath(Properties properties) {
        return scoreFilePath("", properties);
    }

    public static String scoreFilePath(String prefix, Properties properties) {
        return properties.getProperty(prefix+"scoreFilePath");
    }

    public static Map<String, Object> params(Properties properties) {
        return params("", properties);
    }

    public static Map<String, Object> params(String prefix, Properties properties) {
        return params(prefix, properties, null);
    }

    public static Map<String, Object> params(String prefix, Properties properties, Map<String, Object> overwrite) {
        String paramsKey = prefix+"params.";
        Map<String, Object> params = new HashMap<>();
        for(Map.Entry<Object, Object> entry : properties.entrySet()) {
            String key = ((String)entry.getKey());
            if (key.startsWith(paramsKey)) {
                params.put(key.substring(paramsKey.length()), entry.getValue());
            }
        }
        if (overwrite != null) {
            for(Map.Entry<String, Object> entry : overwrite.entrySet()) {
                params.put(entry.getKey(), entry.getValue());
            }
        }
        return params.isEmpty() ? null : params;
    }


    public static CallData callData(String method, Map<String, Object> params) {
        return new CallData(method, params != null && !params.isEmpty() ? params : null);
    }

    public static <T> T call(
            JsonrpcClient client, Class<T> responseType, Address address,
            String method, Map<String, Object> params) {
        return client.request(responseType, "icx_call", new CallParam(address, callData(method, params)));
    }

    public static <T> T call(
            JsonrpcClient client, TypeReference<T> responseType, Address address,
            String method, Map<String, Object> params) {
        return client.request(responseType, "icx_call", new CallParam(address, callData(method, params)));
    }

    static Hash sendTransaction(JsonrpcClient client, Wallet wallet, SendTransactionParam sendTransactionParam) {
        Objects.requireNonNull(client, "client required not null");
        Objects.requireNonNull(wallet, "wallet required not null");
        Objects.requireNonNull(wallet, "sendTransactionParam required not null");

        sendTransactionParam.setFrom(wallet.getAddress());
        if (sendTransactionParam.getTimestamp() == null) {
            sendTransactionParam.setTimestamp(TransactionParam.currentTimestamp());
        }
        if (sendTransactionParam.getStepLimit() == null) {
            sendTransactionParam.setStepLimit(DEFAULT_STEP_LIMIT);
        }
        if (sendTransactionParam.getNid() == null) {
            throw new IllegalArgumentException("nid could not be null");
        }

        Map<String, Object> params = new HashMap<>();
        String serialized;
        try {
            serialized = SendTransactionParamSerializer.serialize(sendTransactionParam, params);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        MessageDigest md;
        try {
            md = MessageDigest.getInstance("SHA3-256");
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }
        byte[] digest = md.digest(serialized.getBytes(StandardCharsets.UTF_8));
        String signature = Base64.getEncoder().encodeToString(wallet.sign(digest));
        params.put("signature", signature);
        return client.request(Hash.class, "icx_sendTransaction", params);
    }

    static void waitForResult(long millis, Hash txh) {
        System.out.println("wait for "+txh);
        try {
            Thread.sleep(millis);
        } catch (InterruptedException ie) {
            ie.printStackTrace();
        }
    }

    public static TransactionResult send(
            JsonrpcClient client, BigInteger nid, Wallet wallet, BigInteger stepLimit, Address address,
            BigInteger valueForPayable, String method, Map<String, Object> params,
            long timeout) {
        return send(client, nid, wallet, stepLimit, address, valueForPayable, method, params, timeout, DEFAULT_RESULT_RETRY_WAIT);
    }
    public static TransactionResult send(
            JsonrpcClient client, BigInteger nid, Wallet wallet, BigInteger stepLimit, Address address,
            BigInteger valueForPayable, String method, Map<String, Object> params,
            long timeout, long resultRetryWait) {
        SendTransactionParam tx = new SendTransactionParam(nid, address, valueForPayable, "call", callData(method, params));
        tx.setStepLimit(stepLimit);
        Hash txh = sendTransaction(client, wallet, tx);
        waitForResult(resultRetryWait*2, txh);
        return result(client, txh, timeout, resultRetryWait);
    }

    public static Address deploy(
            JsonrpcClient client, BigInteger nid, Wallet wallet, BigInteger stepLimit, Address address,
            String scoreFilePath, Map<String, Object> params,
            long timeout) {
        return deploy(client, nid, wallet, stepLimit, address, scoreFilePath, params, timeout, DEFAULT_RESULT_RETRY_WAIT);
    }
    public static Address deploy(
            JsonrpcClient client, BigInteger nid, Wallet wallet, BigInteger stepLimit, Address address,
            String scoreFilePath, Map<String, Object> params,
            long timeout, long resultRetryWait) {
        byte[] content;
        try {
            content = Files.readAllBytes(Path.of(scoreFilePath));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        String contentType;
        if (scoreFilePath.endsWith(".jar")) {
            contentType = "application/java";
        } else if (scoreFilePath.endsWith(".zip")) {
            contentType = "application/zip";
        } else {
            throw new RuntimeException("not supported score file");
        }
        SendTransactionParam tx = new SendTransactionParam(nid, address,null,"deploy", new DeployData(contentType, content, params));
        tx.setStepLimit(stepLimit);
        Hash txh = sendTransaction(client, wallet, tx);
        waitForResult(resultRetryWait*2, txh);
        TransactionResult txr = result(client, txh, timeout, resultRetryWait);
        System.out.println("SCORE address: "+txr.getScoreAddress());
        return txr.getScoreAddress();
    }

    public static TransactionResult transfer(
            JsonrpcClient client, BigInteger nid, Wallet wallet, BigInteger stepLimit, Address address,
            BigInteger value, String message, long timeout) {
        return transfer(client, nid, wallet, stepLimit, address, value, message, timeout, DEFAULT_RESULT_RETRY_WAIT);
    }

    public static TransactionResult transfer(
            JsonrpcClient client, BigInteger nid, Wallet wallet, BigInteger stepLimit, Address address,
            BigInteger value, String message, long timeout, long resultRetryWait) {
        SendTransactionParam tx;
        if (message != null) {
            tx = new SendTransactionParam(nid, address, value, "message", message.getBytes(StandardCharsets.UTF_8));
        } else {
            tx = new SendTransactionParam(nid, address, value, null, null);
        }
        tx.setStepLimit(stepLimit);
        Hash txh = sendTransaction(client, wallet, tx);
        waitForResult(resultRetryWait*2, txh);
        return result(client, txh, timeout, resultRetryWait);
    }

    public static TransactionResult result(JsonrpcClient client, Hash txh, long timeout) {
        return result(client, txh, timeout, DEFAULT_RESULT_RETRY_WAIT);
    }
    public static TransactionResult result(JsonrpcClient client, Hash txh, long timeout, long resultRetryWait) {
        Map<String, Object> params = Map.of("txHash", txh);
        long etime = System.currentTimeMillis() + timeout;
        TransactionResult txr = null;
        while(txr == null) {
            try {
                txr = client.request(TransactionResult.class, "icx_getTransactionResult", params);
            } catch (JsonrpcClient.JsonrpcError e) {
                if (e.getCode() == -31002 /* pending */
                        || e.getCode() == -31003 /* executing */
                        || e.getCode() == -31004 /* not found */) {
                    if (timeout > 0 && System.currentTimeMillis() >= etime) {
                        throw new RuntimeException("timeout");
                    }
                    waitForResult(resultRetryWait, txh);
                } else {
                    throw new RuntimeException(e);
                }
            }
        }
        if (!BigInteger.ONE.equals(txr.getStatus())) {
            TransactionResult.Failure failure = txr.getFailure();
            int revertCode = failure.getCode().intValue();
            String revertMessage = failure.getMessage();
            if (revertCode >= 32) {
                throw new UserRevertedException(revertCode - 32, revertMessage);
            } else {
                throw new RevertedException(revertCode, revertMessage);
            }
        }
        return txr;
    }

    public static BigInteger balance(JsonrpcClient client, Address address) {
        return client.request(BigInteger.class, "icx_getBalance", Map.of("address", address));
    }

    public static <T> T lastBlock(JsonrpcClient client, Class<T> blockType) {
        return client.request(blockType, "icx_getLastBlock", null);
    }

    public static <T> List<T> eventLogs(TransactionResult txr,
                                 String signature,
                                 Address scoreAddress,
                                 Function<TransactionResult.EventLog, T> mapperFunc,
                                 Predicate<T> filter) {
        Predicate<TransactionResult.EventLog> predicate =
                (el) -> el.getIndexed().get(0).equals(signature);
        if (scoreAddress != null) {
            predicate = predicate.and((el) -> el.getScoreAddress().equals(scoreAddress));
        }
        Stream<T> stream = txr.getEventLogs().stream()
                .filter(predicate)
                .map(mapperFunc);
        if(filter != null) {
            stream = stream.filter(filter);
        }
        return stream.collect(Collectors.toList());
    }

}
