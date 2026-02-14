import apiData from '$lib/data/api.json';

export const prerender = true;

export function load() {
	return { modules: apiData.modules };
}
