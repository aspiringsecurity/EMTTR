import { configureStore } from "@reduxjs/toolkit";
import locationReducer from "../redux/slices/locationSlice";
// import { tribunalApi } from "../api/tribunalApi";
// import { setupListeners } from "@reduxjs/toolkit/dist/query";

export const store = configureStore({
  reducer: {
    location: locationReducer,
    // [tribunalApi.reducerPath]: tribunalApi.reducer,
  },
  // middleware: (getDefaultMiddleware) =>
  //   getDefaultMiddleware().concat(tribunalApi.middleware),
});

// setupListeners(store.dispatch);
