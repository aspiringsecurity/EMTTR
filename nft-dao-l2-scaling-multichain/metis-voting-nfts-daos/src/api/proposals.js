const API = import.meta.env.VITE_TRIBUNAL_API;

export const getProposals = async () => {
  try {
    const res = await fetch(`${API}proposals`, {
      method: "GET",
    });

    const content = (await res.json()).data;

    return content;
  } catch (err) {
    console.log({ err });

    return null;
  }
};

export const getProposal = async (id) => {
  try {
    const res = await fetch(`${API}proposal/${id}`, {
      method: "GET",
    });

    const content = (await res.json()).data;

    return content;
  } catch (err) {
    console.log({ err });

    return null;
  }
};

export const createProposal = async (data) => {
  try {
    const res = await fetch(`${API}proposal`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(data),
    });

    const content = (await res.json()).data;

    return content;
  } catch (err) {
    console.log({ err });

    return null;
  }
};

export const updateProposal = async (id, data) => {
  try {
    const res = await fetch(`${API}proposal/${id}`, {
      headers: { "Content-Type": "application/json" },
      method: "PUT",
      body: JSON.stringify(data),
    });

    const content = (await res.json()).data;

    return content;
  } catch (err) {
    console.log({ err });

    return null;
  }
};
